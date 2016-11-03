{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import "clay" Clay
import "base" Data.Monoid ((<>))

main :: IO ()
main = putCss styleSheet

monokaiBackground :: Color
monokaiBackground = rgb 39 40 34

monokaiText :: Color
monokaiText = rgb 208 208 208

warningBackground :: Color
warningBackground = rgb 246 167 63

errorBackground :: Color
errorBackground = rgb 248 66 49

genericMessage :: Css
genericMessage = do
  let s = pt 2
  padding s s s s
  fontWeight bold

  borderRadius s s s s
  color white

styleSheet :: Css
styleSheet = do
  (body <> html) ? do
    background  monokaiBackground
    color       monokaiText
    fontFamily  ["monospace"] [monospace]
    fontSize $  pt 10

    height $    pct 100
    margin      (px 0) (px 0) (px 0) (px 0)

  "#wrapper" ? do
    height $ pct 100
    paddingLeft $ pt 5
    paddingRight $ pt 5

    ":before" ? do
      content $  stringContent ""
      float      floatLeft
      height  $  pct 100

  "#content" ? do
    height $ auto
    ":after" ? do
      content $ stringContent ""
      clear      both

  ".CodeMirror" ?
    height auto

  "a.Warning" ? do
    background warningBackground
    genericMessage

  "a.Error" ? do
    background errorBackground
    genericMessage

