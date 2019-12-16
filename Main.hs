module Main where

import           Control.Applicative
import           Data.Char
import           Data.Maybe
import           Types

-- Helper parsers
charP :: Char -> Parser Char
charP c
  = Parser f
  where
    f "" = Nothing
    f (x : xs)
      | x == c    = Just (xs, x)
      | otherwise = Nothing

stringP :: String -> Parser String
stringP
  = traverse charP

spanP :: (Char -> Bool) -> Parser String
spanP predicate
  = Parser f
  where
    f input = Just (bad, good)
      where
        (good, bad) = span predicate input

whiteSpaceP :: Parser String
whiteSpaceP
  = spanP isSpace

fromNullParsedP :: Parser [a] -> Parser [a]
-- Pre: The given parser must not parse into Nothing
fromNullParsedP (Parser p)
  = Parser f
  where
    f input = if null parsed
              then Nothing
              else Just res
            where
            res@(_, parsed) = fromJust (p input)

-- Actual parsing

pyNone :: Parser PyValue
pyNone
  = PyNone <$ stringP "None"

pyBool :: Parser PyValue
pyBool
  = f <$> (stringP "True" <|> stringP "False")
  where
    f "True"  = PyBool True
    f "False" = PyBool False

pyInt :: Parser PyValue
pyInt
  = f <$> fromNullParsedP (spanP isDigit)
  where
    f = PyInt . read

pyChar :: Parser PyValue
pyChar
  = PyChar <$> ((charP '\'' *>
                foldl1 (<|>) [charP (chr c) | c <- [0 .. 127]]
                <* charP '\'')
                <|>
                (charP '\"' *>
                foldl1 (<|>) [charP (chr c) | c <- [0 .. 127]]
                <* charP '\"'))

pyString :: Parser PyValue
pyString
  = PyString <$> (charP '\"' *> characters <* charP '\"')
  where
    characters = spanP (/= '\"')

pyList :: Parser PyValue
pyList
  = PyList <$> (charP '[' *> whiteSpaceP *> elements <* whiteSpaceP <* charP ']')
  where
    elements = ((:) <$> pyValue <*> many (charP ',' *> whiteSpaceP *> pyValue <* whiteSpaceP))
                <|> pure []

pyValue :: Parser PyValue
pyValue
  = pyNone <|> pyBool <|> pyInt <|> pyChar <|> pyString <|> pyList

-- Input-Output and testing
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
