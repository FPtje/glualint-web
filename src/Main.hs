{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified "glualint-lib" GLuaFixer.LintSettings as Settings
import qualified "glualint-lib" GLuaFixer.AG.ASTLint as Lint
import           "glualint-lib" GLuaFixer.LintMessage (LintMessage (..), sortLintMessages)
import qualified "glualint-lib" GLuaFixer.Util as Util
import qualified "glualint-lib" GLua.Parser as P
import qualified "glualint-lib" GLua.AG.PrettyPrint as PP
import qualified "glualint-lib" GLuaFixer.AG.DarkRPRewrite as DarkRP
import           "reflex-dom"   Reflex.Dom
import           "base"         Data.List (intersperse)
import           "base"         Control.Concurrent (takeMVar, putMVar, MVar, newEmptyMVar, forkIOWithUnmask, ThreadId, killThread, forkIO, threadDelay)
import           "base"         Control.Exception (mask_, AsyncException, catch)
import           "base"         Control.Monad (void, forever)
import           "transformers" Control.Monad.IO.Class (liftIO)
import           "this"         GLualintWeb.Editor


data LintStatus =
    Good -- Good code, no problems!
  | Warnings [LintMessage] -- Shit compiles, but you should worry about some things
  | SyntaxErrors [LintMessage] -- Syntax errors!


type WWidget w = forall t m . (MonadWidget t m) => m (w t)

defConfig :: Settings.LintSettings
defConfig = Settings.defaultLintSettings

lintString :: String -> LintStatus
lintString str = do
  let parsed = Util.parseFile defConfig "input" str

  case parsed of
    Right ([], ast) -> case Lint.astWarnings defConfig ast of
      [] -> Good
      xs -> Warnings $ map ($"input") xs
    Right (warnings, _) -> Warnings warnings
    Left errs -> SyntaxErrors errs

prettyPrint :: String -> String
prettyPrint lua =
  let
    parsed = P.parseGLuaFromString lua
    ast = fst parsed
    ppconf = Settings.lint2ppSetting defConfig
    pretty = PP.prettyprintConf ppconf $ DarkRP.fixOldDarkRPSyntax ast
  in
    pretty


prettyPrintMessages :: [LintMessage] -> [String]
prettyPrintMessages = intersperse "\n" . map show . sortLintMessages

displayMessage :: LintStatus -> [String]
displayMessage Good = []
displayMessage (Warnings msgs) = prettyPrintMessages msgs
displayMessage (SyntaxErrors msgs) = prettyPrintMessages msgs


-- | Does the linter work in a separate thread
lintWorker :: MVar String -> IO (ThreadId, MVar LintStatus)
lintWorker mvCode = do
    mvLintResults <- newEmptyMVar

    worker <- mask_ $ forkIOWithUnmask $ \unmask -> forever $ unmask (work mvLintResults) `catch` \(_ :: AsyncException) -> return ()

    return (worker, mvLintResults)
  where
    work :: MVar LintStatus -> IO ()
    work mvLintResults = do
      code <- takeMVar mvCode

      -- Wait 250 ms before starting to lint
      -- Linting is still heavy, even in another thread.
      threadDelay 250000

      let !linted = lintString code
      putMVar mvLintResults linted
      return ()


-- | Takes an event holding the text to lint
-- Returns the lint messages
lintASync :: (MonadWidget t m) => MVar String -> MVar LintStatus -> ThreadId -> Event t String -> m (Event t LintStatus)
lintASync input result workerId src = performEventAsync $ ffor src $ \strCode throwEvent -> do
  void $ liftIO $ do
    killThread workerId

    putMVar input strCode

    forkIO $ throwEvent =<< takeMVar result

  return ()


main :: IO ()
main = do
  -- Start the worker
  mvCode <- newEmptyMVar
  (workerId, mvLintResults) <- lintWorker mvCode

  mainWidget $ elId "div" "wrapper" $ do

    rec
        ppOnClick <- elId "div" "header" $ do
          prettyPrintButton <- btnPrettyPrint
          onLinted <- lintASync mvCode mvLintResults workerId (updated $ cmValue t)
          lintStatus <- holdDyn Good onLinted

          lintResults <- mapDyn displayMessage lintStatus
          prettyPrinted <- mapDyn prettyPrint $ cmValue t

          let prettyPrintOnClick = tag (current prettyPrinted) prettyPrintButton

          statusMessage lintStatus
          lintResultList lintResults

          return prettyPrintOnClick

        -- codemirror will be added as child to this div later
        elId "div" "content" $ return ()

        t <- codemirror (CodeMirrorConfig "-- Put your Lua here\n" "lua" "monokai" ppOnClick)

    return ()


-- | Pretty print button
btnPrettyPrint :: MonadWidget t m => m (Event t ())
btnPrettyPrint = button "Pretty print"

-- | Status message
statusMessage :: (MonadWidget t m) => Dynamic t LintStatus -> m ()
statusMessage lintStatus =
  let
    status :: LintStatus -> String
    status Good = "Your script is all good!"
    status (Warnings _) = "Your script will not throw syntax errors, but there are some warnings!"
    status (SyntaxErrors _) = "Your script is broken! Check out these errors!"
  in do
    txt <- mapDyn status lintStatus

    el "p" $ dynText txt

-- | Displays a list paragraphs with warnings and/or errors
lintResultList :: (MonadWidget t m) => Dynamic t [String] -> m ()
lintResultList lintResults = void $ el "div" $ simpleList lintResults (el "p" . dynText)

-- | Helper function: element with id
elId :: forall t m a. MonadWidget t m => String -> String -> m a -> m a
elId elementTag i = elWith elementTag $ def & attributes .~ "id" =: i
