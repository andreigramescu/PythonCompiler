module Main where

import Data.Char
import Control.Applicative
import Types

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
  = sequenceA . map charP

spanP :: (Char -> Bool) -> Parser String
spanP predicate
  = Parser f
  where
    f input = if null good
              then Nothing
              else Just (bad, good)
      where
        (good, bad) = span predicate input

-- Actual parsing

pyNone :: Parser PyValue
pyNone
  = (\_ -> PyNone) <$> stringP "None"

pyBool :: Parser PyValue
pyBool
  = f <$> (stringP "True" <|> stringP "False")
  where
    f "True" = PyBool True
    f "False" = PyBool False

pyInt :: Parser PyValue
pyInt
  = f <$> spanP (\x -> (ord x >= 48 && ord x <= 57))
  where
    f = PyInt . read

pyString :: Parser PyValue
pyString
  = PyString <$> (charP '"' *> spanP (/= '"') <* charP '"')

pyValue :: Parser PyValue
pyValue
  = pyNone <|> pyBool <|> pyInt <|> pyString

main :: IO ()
main
  = undefined
