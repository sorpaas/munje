{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import System.IO
import System.Exit
import System.Environment
import System.Console.Readline
import Data.List
import Munje.Parser(parseExpression, parseProgram)
import Munje.Logic(declare, expToDeclaration)

main :: IO ()
main = do
  args <- getArgs
  if null args
     then interactiveShell readExp
     else runFile $ head args

runFile :: String -> IO ()
runFile filePath = do
  inputFile <- openFile filePath ReadMode
  inputText <- hGetContents inputFile
  case parseProgram inputText of
    Left err -> print err
    Right result ->
      let declarations = declare result in do
        putStrLn $ "I declared: " ++ show declarations
        interactiveShellLoop (\aLine -> case parseExpression aLine of
                                 Left err -> print err
                                 Right r -> putStrLn (if any (\x -> x == expToDeclaration r) declarations then "yes" else "no"))

interactiveShellLoop :: (String -> IO ()) -> IO ()
interactiveShellLoop readALine = do
  interactiveShell readALine
  interactiveShellLoop readALine

interactiveShell :: (String -> IO ()) -> IO ()
interactiveShell readALine = do
  maybeLine <- readline "% "
  case maybeLine of
    Nothing -> exitWith ExitSuccess
    Just [] -> return ()
    Just "exit" -> exitWith ExitSuccess
    Just line -> do if isWrongLine line then putStrLn "Unexpected line ..." else
                      if isGirzu line then girzuMoreInput ([line]) else
                        readALine line

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
