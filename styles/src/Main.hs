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


buttonBackground :: Color
buttonBackground = rgb 65 65 65

buttonBackgroundEdge :: Color
buttonBackgroundEdge = rgb 75 75 75

buttonBorder :: Color
buttonBorder = rgb 37 37 37

hoverColor :: Color
hoverColor = rgb 100 100 100

bordersize = pt 2

genericMessage :: Css
genericMessage = do
  padding bordersize bordersize bordersize bordersize
  fontWeight bold

  borderRadius bordersize bordersize bordersize bordersize
  color white

buttonGradient :: Css
buttonGradient =
  backgroundImage $ linearGradient (straight sideTop) [(buttonBackgroundEdge, pct 0), (buttonBackground, pct 50), (buttonBackgroundEdge, pct 100)]

styleSheet :: Css
styleSheet = do
  (body <> html) ? do
    background  monokaiBackground
    color       monokaiText
    fontFamily  ["monospace"] [monospace]
    fontSize $  pt 10

    margin      (px 0) (px 0) (px 0) (px 0)


  "#prettyPrintButton" ? do
    fontFamily  ["monospace"] [monospace]
    position fixed
    right $ pt 5
    top   $ pt 5
    padding (pt 5) (pt 5) (pt 5) (pt 5)
    color monokaiText
    borderColor buttonBorder
    background buttonBackground
    buttonGradient

    borderRadius (pt 4) (pt 4) (pt 4) (pt 4)
    zIndex 10

  "#prettyPrintButton:hover" ?
    borderColor hoverColor

  "#prettyPrintButton:focus" ?
    outline solid (pt 0) black


  "#wrapper" ? do
    height $ pct 100
    paddingLeft $ pt 5
    paddingRight $ pt 5

    ":before" ? do
      content $  stringContent ""
      float      floatLeft
      height  $  pct 100

  "a.lintMessage" ? do
    userSelect none
    cursor pointer

  "a.lintMessage:hover" ? do
    padding (pt 5) (pt 5) (pt 5) (pt 5)
    borderColor buttonBorder
    background buttonBackground
    buttonGradient
    borderRadius (pt 4) (pt 4) (pt 4) (pt 4)


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

