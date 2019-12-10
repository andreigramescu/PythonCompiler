module Types where

data PyValue
  = PyNull
  | PyVar String
  | PyBool Bool
  | PyInt Int
  | PyString String
  | PyArray [PyValue]
  deriving (Show, Eq)

newtype Parser a = Parser { runParser :: String -> Maybe (String, a) }
