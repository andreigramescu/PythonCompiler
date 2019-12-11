module Main where

import Types

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

-- Parsing None
pyNone :: Parser PyValue
pyNone
  = (\_ -> PyNone) <$> stringP "None"

-- TODO: make Parser an instance of Alternative
-- Parsing a boolean value
-- pyBool :: Parser PyValue
-- pyBool
--   = stringP "True" <|> stringP "False"

main :: IO ()
main
  = undefined
