{-# LANGUAGE RecursiveDo #-}

module Main where

import qualified "glualint-lib" GLuaFixer.LintSettings as Settings
import           "glualint-lib" GLuaFixer.LintMessage (LintMessage (..), sortLintMessages)
import qualified "glualint-lib" GLuaFixer.Util as Util
import qualified "glualint-lib" GLua.PSParser as P
import qualified "glualint-lib" GLua.PSLexer as L
import           "safe"         Safe (readMay)
import qualified "reflex"       Reflex
import qualified "reflex-dom"   Reflex.Dom as R
import qualified                Data.Map as Map
import                          Control.Applicative ((<*>), (<$>))
import           "base"         Data.List (intersperse)


data LintStatus =
    Good -- Good code, no problems!
  | Warnings [LintMessage] -- Shit compiles, but you should worry about some things
  | SyntaxErrors [LintMessage] -- Syntax errors!



lintString :: String -> LintStatus
lintString str = do
  let defConfig = Settings.defaultLintSettings

  let parsed = Util.parseFile defConfig "input" str

  case parsed of
    Right ([], _) -> Good
    Right (warnings, _) -> Warnings warnings
    Left errs -> SyntaxErrors errs

prettyPrintMessages :: [LintMessage] -> [String]
prettyPrintMessages = intersperse "\n" . map show . sortLintMessages

displayMessage :: LintStatus -> [String]
displayMessage Good = ["Your script is all good!"]
displayMessage (Warnings msgs) = ["Your script will not throw syntax errors, but there are some warnings:", ""] ++ (prettyPrintMessages msgs)
displayMessage (SyntaxErrors msgs) = ["Your script is broken! Check out these errors!", ""] ++ (prettyPrintMessages msgs)


main :: IO ()
main = R.mainWidget $ do
  rec
      lintResults <- R.mapDyn (displayMessage . lintString) $ R._textArea_value t

      R.el "div" $ R.simpleList lintResults (R.el "div" . R.dynText)
      -- R.el "div" $ R.dynText lintResult
      t <- R.el "div" $ R.textArea $ R.def
        R.& R.textAreaConfig_initialValue R..~ "-- Put your Lua here"

      return ()

  return ()
