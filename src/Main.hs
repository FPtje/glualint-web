{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           "base"         Control.Concurrent
import           "base"         Control.Monad
import qualified "ghcjs-base"   Data.JSString as JS
import           "glualint" GLua.AG.Token (Region(..))
import qualified "glualint" GLuaFixer.LintSettings as Settings
import           "glualint" GLuaFixer.LintMessage (LintMessage (..), Severity (..), issueDescription, sortLintMessages)
import qualified "glualint" GLuaFixer.Interface as GLua
import qualified "glualint" GLua.Parser as P
import qualified "glualint" GLua.AG.PrettyPrint as PP
import           "lens"         Control.Lens hiding (view)
import           "miso"         Miso (App(..), Transition)
import qualified "miso"         Miso
import           "miso"         Miso.Html
import qualified "miso"         Miso.String as Miso
import qualified "this"         GLualintWeb.Editor as CodeMirror
import           "uu-parsinglib"  Text.ParserCombinators.UU.BasicInstances (LineColPos (..))

data LintStatus =
    Good -- Good code, no problems!
  | Warnings [LintMessage] -- Shit compiles, but you should worry about some things
  | SyntaxErrors [LintMessage] -- Syntax errors!
  deriving ( Eq )

data Model
   = Model
     { _mCodeMirror       :: !CodeMirror.Model
     , _mLintStatus       :: !LintStatus
     , _mCodeMirrorValue  :: !JS.JSString
     , _mBackgroundLinter :: !(Maybe ThreadId)
     } deriving ( Eq )

makeLenses ''Model

data Action
   = CodeMirrorAction !CodeMirror.Action
   | OnEditorChanged !JS.JSString
   | PrettyPrint
   | LintMessageClicked !Region
   | RegisterLintThread !ThreadId
   | CodeLinted !LintStatus
   | NoOp

initialModel :: Model
initialModel =
    Model
    { _mCodeMirror       = CodeMirror.initialModel
    , _mLintStatus       = Good
    , _mCodeMirrorValue  = "-- Put your Lua here\n"
    , _mBackgroundLinter = Nothing
    }

main :: IO ()
main = do
    Miso.startApp App
      { initialAction = NoOp
      , model         = initialModel
      , update        = Miso.fromTransition . updateModel
      , view          = viewModel
      , events        = Miso.defaultEvents
      , subs          = [ ]
      , logLevel      = Miso.Off
      , mountPoint    = Nothing
      }

updateModel :: Action -> Transition Action Model ()
updateModel = \case
    CodeMirrorAction act ->
      zoom mCodeMirror $
        CodeMirror.updateModel codeMirrorIface act

    OnEditorChanged str -> do
      mCodeMirrorValue .= str
      mbBackgroundLinter <- use mBackgroundLinter
      lua <- use mCodeMirrorValue

      Miso.scheduleSub $ \sink -> do
        -- Kill any running threads, only one linting process at a time
        forM_ mbBackgroundLinter $ \tid ->
          killThread tid

        tid <- forkIO $ do
          -- Wait 250 ms before starting to lint
          -- Linting is still heavy, even in another thread.
          threadDelay 250000

          let !linted = lintString $ Miso.fromMisoString lua
          sink $ CodeLinted linted

        sink $ RegisterLintThread tid

    PrettyPrint -> do
      str <- use mCodeMirrorValue

      zoom mCodeMirror $
        CodeMirror.updateModel codeMirrorIface $
          CodeMirror.SetValue $
          prettyPrint $
          Miso.fromMisoString str

    LintMessageClicked rg ->
      zoom mCodeMirror $
        CodeMirror.updateModel codeMirrorIface $ CodeMirror.SelectRegion rg

    RegisterLintThread tid ->
      mBackgroundLinter .= Just tid

    CodeLinted lintStatus -> do
      mLintStatus .= lintStatus

      let messages = case lintStatus of
            Good -> []
            Warnings msgs -> msgs
            SyntaxErrors msgs -> msgs

      zoom mCodeMirror $
            CodeMirror.updateModel codeMirrorIface $
              CodeMirror.SetLintMessages messages

    NoOp -> pure ()

viewModel :: Model -> View Action
viewModel m =
  div_ []
  [ button_
    [ id_ "prettyPrintButton"
    , type_ "button"
    , onClick PrettyPrint
    ]
    [ text "Pretty print"]
  , div_ [ id_ "header" ]
    [ p_ []
      [ text $ statusMessage $ m ^. mLintStatus ]
    , div_ [] $
      Miso.withFoldable (displayMessage $ m ^. mLintStatus) viewLintMessage
    ]
  , CodeMirror.viewModel codeMirrorIface (m ^. mCodeMirror)
  ]

viewLintMessage :: LintMessage -> View Action
viewLintMessage = \case
    lm@(LintMessage severity region _ _) ->
      p_ []
      [ a_ [ class_ "lintMessage", onClick $ LintMessageClicked region ]
        [ a_ [ class_ (severityStr severity) ] [ text (severityStr severity) ]
        , text $ prettyPrintMessage lm
        ]
      ]
  where
    severityStr :: Severity -> JS.JSString
    severityStr = \case
      LintWarning -> "Warning"
      LintError -> "Error"

defConfig :: Settings.LintSettings
defConfig = Settings.defaultLintSettings

lintString :: String -> LintStatus
lintString str = do
  case GLua.lex defConfig "input" str of
    Left lexErrors -> SyntaxErrors lexErrors
    Right mtokens ->
      case GLua.parse defConfig "input" mtokens of
        Left parseErrors -> SyntaxErrors parseErrors
        Right ast ->
          case GLua.lexiconLint "input" defConfig mtokens ++ GLua.astLint "input" defConfig ast of
            [] -> Good
            warnings -> Warnings warnings

prettyPrint :: String -> String
prettyPrint lua =
  let
    parsed = P.parseGLuaFromString lua
    ast = fst parsed
    ppconf = Settings.lint2ppSetting defConfig
    pretty = PP.prettyprintConf ppconf ast
  in
    pretty

prettyPrintMessage :: LintMessage -> JS.JSString
prettyPrintMessage = \case
    LintMessage _severity (Region (LineColPos l _ _) _) msg _ -> pretty l $ issueDescription msg
  where
    pretty :: Int -> String -> JS.JSString
    pretty l msg = "Line " <> Miso.toMisoString (succ l) <> ": " <> Miso.toMisoString msg


statusMessage :: LintStatus -> JS.JSString
statusMessage = \case
    Good ->
      "Your script is all good!"
    (Warnings _) ->
      "Your script will not throw syntax errors, but there are some warnings!"
    (SyntaxErrors _) ->
      "Your script is broken! Check out these errors!"

displayMessage :: LintStatus -> [LintMessage]
displayMessage Good = []
displayMessage (Warnings msgs) = sortLintMessages msgs
displayMessage (SyntaxErrors msgs) = sortLintMessages msgs

codeMirrorIface :: CodeMirror.Interface Action
codeMirrorIface =
    CodeMirror.Interface
    { CodeMirror.uniqueId   = "content"
    , CodeMirror.passAction = CodeMirrorAction
    , CodeMirror.onChanged  = OnEditorChanged
    }
