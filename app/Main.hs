{-# LANGUAGE LambdaCase #-}
module Main (main) where

import Regex
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import Prelude

main :: IO ()
main = do
  input <-
    getArgs >>= \case
      [] -> do
        pn <- getProgName
        putStrLn $ "Usage: " <> pn <> " <the.regexes>"
        exitFailure
      (fn:_) -> readFile fn
  print $ solve input
