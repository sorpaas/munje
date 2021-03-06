module Munje.Parser
       ( parseProgram
       , parseExpression
       , Bridi(Bridi)
       , Girzu(Girzu)
       , Sumti
       , Selbri
       , Expression) where

import Text.Parsec hiding (State, sourceName)
import Text.Parsec.Indent
import Control.Monad.State

type Expression = Either Bridi Girzu
data Bridi = Bridi Selbri [Sumti]
           deriving (Show)
data Girzu = Girzu [Exposed] [Bridi]
           deriving (Show)

type Exposed = Selbri
type Selbri = String
type Sumti = Selbri

parseExpression :: String -> Either ParseError Expression
parseExpression = iParse aExpression "example"

parseProgram :: String -> Either ParseError [Expression]
parseProgram = iParse aProgram "example"

type IParser a = ParsecT String () (State SourcePos) a
iParse :: IParser a -> SourceName -> String -> Either ParseError a
iParse aParser sourceName input =
  runIndent sourceName $ runParserT aParser () sourceName input

aProgram :: IParser [Expression]
aProgram = many1 aExpression

aExpression :: IParser Expression
aExpression = choice [do
                         girzu <- aGirzu
                         return $ Right girzu,
                      do
                         bridi <- aBridi
                         return $ Left bridi
                     ]

aGirzu :: IParser Girzu
aGirzu = do
  b <- withBlock Girzu definitionLine aBridi
  spaces
  return b

definitionLine :: IParser [Exposed]
definitionLine = do
  string "define"
  spaces
  b <- aSelbri `sepBy` (do
                           char ','
                           spaces)
  spaces
  return b

aBridi :: IParser Bridi
aBridi = do
  selbri <- aSelbri
  char ' '
  sumtis <- aSelbri `sepBy` (char ' ')
  spaces
  return (Bridi selbri sumtis)

aSelbri :: IParser Selbri
aSelbri = choice [ do
                      a <- many1 alphaNum
                      return a
                 ,
                   do
                      char '('
                      b <- oneOf ">-*"
                      char ')'
                      return [b]
                 ]
