{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import System.IO
import System.Environment
import System.Console.Readline
import Data.List
import Parser(parseExpression, parseProgram)

main :: IO ()
main = do
  args <- getArgs
  if null args
     then interactiveShell
     else runFile $ head args

runFile :: String -> IO ()
runFile filePath = do
  inputFile <- openFile filePath ReadMode
  inputText <- hGetContents inputFile
  case parseProgram inputText of
    Left err -> print err
    Right result -> putStrLn $ "I parsed: " ++ show result

interactiveShell :: IO ()
interactiveShell = do
  maybeLine <- readline "% "
  case maybeLine of
    Nothing -> return ()
    Just [] -> interactiveShell
    Just "exit" -> return ()
    Just line -> do if isWrongLine line then putStrLn "Unexpected line ..." else
                      if isGirzu line then girzuMoreInput ([line]) else
                        readExp line
                    interactiveShell

isWrongLine :: String -> Bool
isWrongLine (h : _) =
  h == ' ' || h == '\t'

isGirzu :: String -> Bool
isGirzu (stripPrefix "define" -> Just _) = True
isGirzu _ = False

girzuMoreInput :: [String] -> IO ()
girzuMoreInput acc = do
  maybeLine <- readline ". "
  case maybeLine of
    Nothing -> readExp $ unlines acc
    Just [] -> readExp $ unlines acc
    Just line -> girzuMoreInput (acc ++ [line])

readExp :: String -> IO ()
readExp inputText =
  case parseExpression inputText of
    Left err -> print err
    Right result -> putStrLn $ "I parsed: " ++ show result
