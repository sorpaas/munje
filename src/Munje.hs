module Main where

import Parser(parse)

inputText :: String
inputText = unlines [ "define factorial",
                      "    factorial 0 1",
                      "    factorial n y",
                      "    (>) n 0",
                      "    (-) n 1 np",
                      "    factorial np yp",
                      "    (*) yp n y"
                    ]

main :: IO ()
main = do
  putStrLn inputText
  case parse inputText of
    Left err -> print err
    Right result -> putStrLn $ "I parsed: " ++ show result
