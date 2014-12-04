{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO
import Parser(parseExpression, parseProgram)

main :: IO ()
main = do
  inputFile <- openFile "examples/factorial.emj" ReadMode
  inputText <- hGetContents inputFile
  case parseProgram inputText of
    Left err -> print err
    Right result -> putStrLn $ "I parsed: " ++ show result
