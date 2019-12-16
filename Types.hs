module Types where

import           Control.Applicative

data PyValue
  = PyNone
  | PyBool Bool
  | PyInt Int
  | PyChar Char
  | PyString String
  | PyList [PyValue]
  deriving (Show, Eq)

-- Parsing type and instances
newtype Parser a = Parser { runParser :: String -> Maybe (String, a) }

instance Functor Parser where
  fmap f (Parser p) = Parser (\input -> do
                        (input', x) <- p input
                        return (input', f x))

instance Applicative Parser where
  pure x  = Parser (\input -> Just (input, x))
  Parser p1 <*> Parser p2 =
    Parser (\input -> do
      (input', f) <- p1 input
      (input'', x) <- p2 input'
      return (input'', f x))

instance Alternative Parser where
  empty = Parser (const Nothing)
  (Parser p1) <|> (Parser p2)
    = Parser (\input -> p1 input <|> p2 input)
