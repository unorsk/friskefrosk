{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Friskefrosk.Parse (parse)
import Text.Megaparsec.Error (errorBundlePretty)

main :: IO ()
main = do
  let source = "<repl>"
  case parse source "Parent(1, 2)." of
    Left e -> putStr $ errorBundlePretty e
    Right rules -> print rules
