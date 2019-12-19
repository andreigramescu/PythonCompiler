module Types where

import           Control.Applicative

type Name = String

data PyValue
  = PyNone
  | PyBool Bool
  | PyInt Int
  | PyChar Char
  | PyString String
  | PyList [PyValue]
  | PyDict [(PyValue, PyValue)]
  | PyVariable Name
  | PyFunctionCall Name [PyValue]
  deriving (Show, Eq)

data ArithmeticExpression
  = ArithmeticValue PyValue --Will be either int, var or function call
  | Plus ArithmeticExpression ArithmeticExpression
  | Minus ArithmeticExpression ArithmeticExpression
  | Multiply ArithmeticExpression ArithmeticExpression
  | Divide ArithmeticExpression ArithmeticExpression
  | Mod ArithmeticExpression ArithmeticExpression
  | Pow ArithmeticExpression ArithmeticExpression
  deriving (Eq)

data BooleanExpression
  = Atom PyValue -- Will always be a PyBool
  | Compare Char ArithmeticExpression ArithmeticExpression
  | BooleanFunctionCall Name [PyValue]
  | Not BooleanExpression
  | And BooleanExpression BooleanExpression
  | Or BooleanExpression BooleanExpression
  deriving (Show, Eq)

data Procedure
  = Assignment Name PyValue
  | If BooleanExpression [Procedure]
  | While BooleanExpression [Procedure]
  | For String [PyValue] [Procedure]
  | VoidFunctionCall Name [PyValue]
  deriving (Show, Eq)

data FunctionDeclaration
  = FunctionDeclaration {
    getName      :: Name,
    getArguments :: [Name],
    getBody      :: [Procedure]
  }
  deriving (Show, Eq)

-- For clarity
instance Show ArithmeticExpression where
  show (ArithmeticValue v)        = show v
  show (Plus e1 e2)     = "(" ++ show e1 ++ ") + (" ++ show e2 ++ ")"
  show (Minus e1 e2)    = "(" ++ show e1 ++ ") - (" ++ show e2 ++ ")"
  show (Multiply e1 e2) = "(" ++ show e1 ++ ") * (" ++ show e2 ++ ")"
  show (Divide e1 e2)   = "(" ++ show e1 ++ ") / (" ++ show e2 ++ ")"
  show (Mod e1 e2)      = "(" ++ show e1 ++ ") % (" ++ show e2 ++ ")"
  show (Pow e1 e2)      = "(" ++ show e1 ++ ") ** (" ++ show e2 ++ ")"


-- Parsing type and instances
newtype Parser a
  = Parser {
  runParser :: String -> Maybe (String, a)
  }

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
