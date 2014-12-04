{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO
import System.Environment
import Parser(parseExpression, parseProgram)

main :: IO ()
main = do
  args <- getArgs
  if null args
     then putStrLn "Oh, there should be an interactive shell ..."
     else runFile $ head args

runFile :: String -> IO ()
runFile filePath = do
  inputFile <- openFile filePath ReadMode
  inputText <- hGetContents inputFile
  case parseProgram inputText of
    Left err -> print err
    Right result -> putStrLn $ "I parsed: " ++ show result
