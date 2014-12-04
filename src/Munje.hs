{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO
import Parser(parse)

main :: IO ()
main = do
  inputFile <- openFile "examples/factorial.emj" ReadMode
  inputText <- hGetContents inputFile
  case parse inputText of
    Left err -> print err
    Right result -> putStrLn $ "I parsed: " ++ show result
