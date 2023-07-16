{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module GLualintWeb.Editor where

import           "base"           Control.Monad
import           "lens"           Control.Lens
import           "glualint"   GLua.AG.Token (Region(..))
import           "glualint"   GLuaFixer.LintMessage (LintMessage (..), Severity (..), issueDescription)
import qualified "ghcjs-base"     Data.JSString as JS
import qualified "ghcjs-base"     GHCJS.Foreign.Callback as JSCallback
import           "ghcjs-base"     GHCJS.Foreign.Callback ( Callback )
import           "ghcjs-base"     GHCJS.Types ( IsJSVal, JSVal )
import qualified "miso"            Miso
import           "miso"            Miso.Html
import qualified "miso"            Miso.String as Miso
import           "uu-parsinglib"   Text.ParserCombinators.UU.BasicInstances (LineColPos (..))


data Model
   = Model
     { _mCodeMirror :: !(Maybe CodeMirrorWidget)
     } deriving (Eq)

newtype CodeMirrorWidget = CodeMirrorWidget JSVal
instance IsJSVal CodeMirrorWidget

instance Eq CodeMirrorWidget where
  _ == _ = True

initialModel :: Model
initialModel =
    Model
    { _mCodeMirror = Nothing
    }

data Interface action
   = Interface
     { uniqueId   :: !Miso.MisoString
     , passAction :: Action -> action
     , onChanged  :: JS.JSString -> action
     }

data Action
   = OnCreated
   | WidgetCreated !CodeMirrorWidget
   | SetValue !String
   | SetLintMessages ![LintMessage]
   | SelectRegion !Region

makeLenses ''Model

updateModel
    :: Interface action
    -> Action
    -> Miso.Transition action Model ()
updateModel iface = \case
    OnCreated -> Miso.scheduleSub $ \sink -> do
      parent <- getElementById $ uniqueId iface
      widget <- createWidget parent "-- Put your Lua here\n" "lua" "monokai"

      addOnChangeEvent iface sink widget

      sink $ passAction iface $ WidgetCreated widget

    WidgetCreated w ->
      mCodeMirror .= Just w

    SetValue val -> do
      mbCodeMirror <- use mCodeMirror

      forM_ mbCodeMirror $ \codeMirror -> do
        Miso.scheduleIO $ do
          let jsCode = Miso.toMisoString val
          cmSetText codeMirror jsCode

          pure $ onChanged iface jsCode

    SetLintMessages lintMessages -> do
      mbCodeMirror <- use mCodeMirror

      forM_ mbCodeMirror $ \codeMirror ->
        Miso.scheduleSub $ \_ -> do
          cmResetLintMessages codeMirror

          forM_ lintMessages $ \lintMessage ->
            addLintMessage codeMirror lintMessage

          cmRefresh codeMirror

    SelectRegion (Region (LineColPos ls cs _) (LineColPos le ce _)) -> do
      mbCodeMirror <- use mCodeMirror

      forM_ mbCodeMirror $ \codeMirror ->
        Miso.scheduleSub $ \_ ->
          cmSelectRegion codeMirror ls cs le ce

viewModel :: Interface action -> Model -> Miso.View action
viewModel iface _model =
    div_
      [ id_ $ uniqueId iface
      , Miso.onCreated $ passAction iface OnCreated
      ]
      []

foreign import javascript unsafe "$r = createCodeMirror($1, $2, $3, $4);"
  createWidget
    :: JSVal
    -> JS.JSString
    -> JS.JSString
    -> JS.JSString
    -> IO CodeMirrorWidget

foreign import javascript unsafe "$r = document.getElementById($1);"
  getElementById :: Miso.MisoString -> IO JSVal

foreign import javascript unsafe "$1.setValue($2);"
  cmSetText :: CodeMirrorWidget -> JS.JSString -> IO ()

addOnChangeEvent
    :: Interface action
    -> (action -> IO ())
    -> CodeMirrorWidget
    -> IO ()
addOnChangeEvent iface sink widget = do
    callback <- JSCallback.asyncCallback cbOnChange
    jsAddOnChangeEvent widget callback
  where
    cbOnChange :: IO ()
    cbOnChange = do
      txt <- cmGetText widget
      sink $ onChanged iface txt

foreign import javascript unsafe "$1.on(\"change\", $2);"
  jsAddOnChangeEvent :: CodeMirrorWidget -> Callback a -> IO ()

foreign import javascript unsafe "$r = $1.getValue();"
  cmGetText :: CodeMirrorWidget -> IO JS.JSString

foreign import javascript unsafe "resetMessages($1)"
  cmResetLintMessages :: CodeMirrorWidget -> IO ()

addLintMessage :: CodeMirrorWidget -> LintMessage -> IO ()
addLintMessage widget = \case
    LintMessage severity (Region (LineColPos ls cs _) (LineColPos le ce _)) msg _file ->
      cmAddLintMessage widget ls cs le ce (severityStr severity) $ Miso.toMisoString $
      issueDescription msg
  where
    severityStr :: Severity -> JS.JSString
    severityStr = \case
      LintWarning -> "warning"
      LintError -> "error"

foreign import javascript unsafe "addLintMessage($1, $2, $3, $4, $5, $6, $7)"
  cmAddLintMessage
      :: CodeMirrorWidget
      -> Int -- startLine
      -> Int -- start column
      -> Int -- end line
      -> Int -- end column
      -> JS.JSString -- severity (error or warning)
      -> JS.JSString -- message
      -> IO ()

foreign import javascript unsafe "cmSelectRegion($1, $2, $3, $4, $5)"
  cmSelectRegion
      :: CodeMirrorWidget
      -> Int -- startLine
      -> Int -- start column
      -> Int -- end line
      -> Int -- end column
      -> IO ()

foreign import javascript unsafe "$1.refresh();$1.performLint();"
  cmRefresh :: CodeMirrorWidget -> IO ()
