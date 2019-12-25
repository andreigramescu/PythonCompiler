module Main where

import           Types
import           Parsers

parseFile :: FilePath -> Parser a -> IO (Maybe a)
parseFile fileName parser
  = do
      input <- readFile fileName
      return (snd <$> runParser parser input)

runTest :: IO (Maybe PyValue)
runTest
  = parseFile "./testInput.txt" pyValue

main :: IO ()
main
  = undefined
