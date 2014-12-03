import Text.Parsec hiding (State)
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

type IParser a = ParsecT String () (State SourcePos) a
iParse :: IParser a -> SourceName -> String -> Either ParseError a
iParse aParser sourceName input =
  runIndent sourceName $ runParserT aParser () sourceName input

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
  case iParse aGirzu "example" inputText of
    Left err -> print err
    Right result -> putStrLn $ "I parsed: " ++ show result

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
