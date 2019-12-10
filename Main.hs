module Main where

import Types

instance Functor Parser where
  fmap = undefined

instance Applicative Parser where
  pure  = undefined
  <*>   = undefined

charP :: Char -> Parser Char
charP c
  = Parser f
  where
    f "" = Nothing
    f (x : xs)
      | x == c    = Just (xs, x)
      | otherwise = Nothing

-- TODO: make Parser a Functor and an Applicative
-- stringP :: String -> Parser String
-- stringP
--   = sequenceA . map charP

-- Parsing null
pyNull :: Parser PyValue
pyNull
  = undefined

main :: IO ()
main
  = undefined
