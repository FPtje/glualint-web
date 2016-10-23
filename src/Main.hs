module Main where

import "glualint-lib" GLuaFixer.LintSettings
import "glualint-lib" GLuaFixer.Util
import "glualint-lib" GLua.PSParser
import "glualint-lib" GLua.PSLexer



main :: IO ()
main = do
  putStrLn "Linting the following Lua: \"local anus = 3 print(anus)\""
  let defConfig = defaultLintSettings
  let str = "local anus = 3 print(anus)"

  -- let Right lexed = execParseTokens str
  -- let Right parsed = parseGLua lexed

  let Right parsed = parseFile defConfig "input" str

  print $ fst parsed

