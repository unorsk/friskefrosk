{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (pack)
import Friskefrosk.Parse (parse)
import System.Console.Haskeline
import Text.Megaparsec.Error (errorBundlePretty)

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "frosk> "
      case minput of
        Nothing -> pure ()
        Just input -> do
          case parse "<repl>" (pack input) of
            Left e -> outputStrLn $ errorBundlePretty e
            Right rules -> outputStrLn $ show rules
          loop
