module Parser where

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
type Sumti = String

parse :: String -> Either ParseError Girzu
parse = iParse aGirzu "example"

type IParser a = ParsecT String () (State SourcePos) a
iParse :: IParser a -> SourceName -> String -> Either ParseError a
iParse aParser sourceName input =
  runIndent sourceName $ runParserT aParser () sourceName input

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
