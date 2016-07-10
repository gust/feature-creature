module Git.Diff
( FileModification (..)
, ParseResult
, parseStatusDiff
) where

import Text.ParserCombinators.Parsec
import Control.Monad (void)

data FileModification = Added FilePath
                      | Copied FilePath
                      | Deleted FilePath
                      | Modified FilePath
                      | Renamed FilePath
                      | TypeChanged FilePath
                      | Unmerged FilePath
                      | Unknown FilePath
                      | Broken FilePath
                      | Unrecognized Char FilePath
  deriving (Show, Eq)

type ParseResult = Either ParseError FileModification

parseStatusDiff :: [String] -> [ParseResult]
parseStatusDiff = map parseStatusDiff'

parseStatusDiff' :: String -> ParseResult
parseStatusDiff' d = parse parseFileModification "fileModification" d

parseFileModification :: Parser FileModification
parseFileModification = do
  modType <- oneOf "ACDMRTUXB"
  void $ many1 $ space
  fileName <- many1 $ anyChar
  return $ fileModification modType fileName

fileModification :: Char -> String -> FileModification
fileModification 'A' fileName = Added fileName
fileModification 'C' fileName = Copied fileName
fileModification 'D' fileName = Deleted fileName
fileModification 'M' fileName = Modified fileName
fileModification 'R' fileName = Renamed fileName
fileModification 'T' fileName = TypeChanged fileName
fileModification 'U' fileName = Unmerged fileName
fileModification 'X' fileName = Unknown fileName
fileModification 'B' fileName = Broken fileName
fileModification abbr fileName = Unrecognized abbr fileName
