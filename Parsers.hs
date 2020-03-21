module Parsers where

import           Types
import           Control.Applicative
import           Data.Char
import           Data.Maybe

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

instance Monad Parser where
  return x = Parser (\input -> Just (input, x))
  p >>= f = Parser (\input -> do
                      (extra, val) <- runParser p input
                      (extra', val') <- runParser (f val) extra
                      return (extra', val'))


--  + + + + + + + + + Helper parsers + + + + + + + + +

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

fromEmptyP :: Parser [a] -> Parser [a]
-- Pre: The given parser must not parse into Nothing
fromEmptyP (Parser p)
  = Parser f
  where
    f input = if null parsed
              then Nothing
              else Just res
            where
            res@(_, parsed) = fromJust (p input)

removeBracketsP :: Parser a -> Parser a
removeBracketsP p
  = p <|> bracketsP (removeBracketsP p)
    where
      bracketsP p = charP '(' *> whiteSpaceP *> p <* whiteSpaceP <* charP ')'

(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) = liftA2 (:)

-- + + + + + + + + + Actual parsing + + + + + + + + +

-- Parsing values and objects
pyNone' :: Parser PyValue
pyNone'
  = PyNone <$ stringP "None"

pyBool' :: Parser PyValue
pyBool'
  = PyBool True <$ stringP "True"
  <|> PyBool False <$ stringP "False"

pyInt' :: Parser PyValue
pyInt'
  = (PyInt . read) <$>
  (nextDigitsP <|> (charP '-') <:> nextDigitsP)
  where
    nextDigitsP = fromEmptyP (spanP isDigit)

pyChar' :: Parser PyValue
pyChar'
  = PyChar <$> ((charP '\'' *>
                foldl1 (<|>) [charP (chr c) | c <- [0 .. 127]]
                <* charP '\'')
                <|>
                (charP '\"' *>
                foldl1 (<|>) [charP (chr c) | c <- [0 .. 127]]
                <* charP '\"'))

pyString' :: Parser PyValue
pyString'
  = PyString <$> (charP '\"' *> characters <* charP '\"')
  where
    characters = spanP (/= '\"')

pyList' :: Parser PyValue
pyList'
  = PyList [] <$ (charP '[' <* whiteSpaceP <* charP ']') <|>
    PyList <$> (charP '[' *> whiteSpaceP *> elements <* whiteSpaceP <* charP ']')
  where
    sepByP sep e = e <:> (many (sep *> e))
    elements = sepByP (whiteSpaceP *> charP ',' <* whiteSpaceP) pyValue

pyVariable' :: Parser PyValue
pyVariable'
  = PyVariable <$> fromEmptyP (Parser (\input -> do
                                            (extra, name) <- runParser (spanP isAlpha) input
                                            if name == "or" || name == "and" || name == "not"
                                            then return (input, "")
                                            else return (extra, name)))

pyDict' :: Parser PyValue
pyDict'
  = PyDict <$> (charP '{' *> whiteSpaceP *> bindings <* whiteSpaceP <* charP '}')
    where
      bindings = (pyPair <* whiteSpaceP) <:> many (charP ',' *> whiteSpaceP *> pyPair <* whiteSpaceP) <|> pure []
      pyPair
        = Parser (\input -> do
            (extra, key) <- runParser pyValue input
            (extra', val) <- runParser (whiteSpaceP *> charP ':' *> whiteSpaceP *> pyValue) extra
            return (extra', (key, val)))

pyFunctionCall' :: Parser PyValue
pyFunctionCall'
  = PyFunctionCall <$>
    fromEmptyP (spanP isAlpha) <*>
    (([] <$ (whiteSpaceP *> charP '(' *> whiteSpaceP *> charP ')'))  <|>
    (whiteSpaceP *> charP '(' *> whiteSpaceP *> arguments <* whiteSpaceP <* charP ')'))
    where
      arguments = (pyValue <* whiteSpaceP) <:>
                many (charP ',' *> whiteSpaceP *> pyValue <* whiteSpaceP)

pyNone         = removeBracketsP pyNone'
pyBool         = removeBracketsP pyBool'
pyInt          = removeBracketsP pyInt'
pyChar         = removeBracketsP pyChar'
pyString       = removeBracketsP pyString'
pyList         = removeBracketsP pyList'
pyVariable     = removeBracketsP pyVariable'
pyDict         = removeBracketsP pyDict'
pyFunctionCall = removeBracketsP pyFunctionCall'

pyValue :: Parser PyValue
pyValue
  = pyNone <|> pyBool <|> pyInt <|> pyChar <|>
    pyString <|> pyList <|> pyDict <|> pyFunctionCall <|> pyVariable
    <|> (PyArithmExpr <$> pyArithmExpr)
    <|> (PyBoolExpr <$> pyBoolExpr)

--Parsing arithmetic expressions
pyArithmVal :: Parser ArithmExpr
pyArithmVal
  = ArithmVal <$> (pyInt <|> pyFunctionCall <|> pyVariable)

fromInfixP :: Parser a -> Parser b -> Parser c -> (a -> b -> d) -> Parser d
fromInfixP t1P t2P op build
  = do
      t1 <- t1P
      whiteSpaceP
      op
      whiteSpaceP
      t2 <- t2P
      return (build t1 t2)

pyArithmExpr :: Parser ArithmExpr
pyArithmExpr = fromRA <$> (fromInfixP termP pyArithmExpr (charP '+') Plus
        <|> fromInfixP termP pyArithmExpr (charP '-') Minus)
        <|> termP
      where
        fromRA (Plus a (Plus b c)) = Plus (Plus a b) c
        fromRA (Plus a (Minus b c)) = Minus (Plus a b) c
        fromRA (Minus a (Plus b c)) = Plus (Minus a b) c
        fromRA (Minus a (Minus b c)) = Minus (Minus a b) c
        fromRA x = x

termP :: Parser ArithmExpr
termP = fromRA <$> (fromInfixP factor termP (charP '*') Mul
        <|> fromInfixP factor termP (charP '/') Div
        <|> fromInfixP factor termP (charP '%') Mod)
        <|> factor
      where
        fromRA (Mul a (Mul b c)) = Mul (Mul a b) c
        fromRA (Mul a (Div b c)) = Div (Mul a b) c
        fromRA (Div a (Mul b c)) = Mul (Div a b) c
        fromRA (Div a (Div b c)) = Div (Div a b) c
        fromRA x = x

factor :: Parser ArithmExpr
factor = fromInfixP base factor (stringP "**") Pow
        <|> base

base :: Parser ArithmExpr
base = charP '(' *> whiteSpaceP *> pyArithmExpr <* whiteSpaceP <* charP ')'
        <|> pyArithmVal

-- Parsing boolean expressions
pyBoolExpr :: Parser BoolExpr
pyBoolExpr = fromRA <$> fromInfixP disjunctP pyBoolExpr (stringP "or") Or
          <|>  disjunctP
          where
            fromRA (Or a (Or b c)) = Or (Or a b) c
            fromRA (Or a (And b c)) = And (Or a b) c
            fromRA (And a (Or b c)) = Or (And a b) c
            fromRA (And a (And b c)) = And (And a b) c
            fromRA x = x

disjunctP :: Parser BoolExpr
disjunctP = fromRA <$> fromInfixP conjuctP disjunctP (stringP "and") And
          <|>  conjuctP
          where
            fromRA (Or a (Or b c)) = Or (Or a b) c
            fromRA (Or a (And b c)) = And (Or a b) c
            fromRA (And a (Or b c)) = Or (And a b) c
            fromRA (And a (And b c)) = And (And a b) c
            fromRA x = x

conjuctP :: Parser BoolExpr
conjuctP = Not <$> (stringP "not " *> whiteSpaceP *> pyAtom)
          <|> pyAtom

pyAtom :: Parser BoolExpr
pyAtom
  = charP '(' *> whiteSpaceP *> pyBoolExpr <* whiteSpaceP <* charP ')'
    <|> pyComp
    <|> Atom <$> pyValue

pyComp :: Parser BoolExpr
pyComp
  = Comp <$> pyValue <* whiteSpaceP
  <*> foldl1 (<|>) (map stringP comps) <* whiteSpaceP
  <*> pyValue

-- Parsing procedures
pyAssignment :: Parser Procedure
pyAssignment
  = undefined

pyIf :: Parser Procedure
pyIf
  = undefined

pyWhile :: Parser Procedure
pyWhile
  = undefined

pyFor :: Parser Procedure
pyFor
  = undefined

pyVoidFunctionCall :: Parser Procedure
pyVoidFunctionCall
  = undefined

pyProcedure :: Parser Procedure
pyProcedure
  = undefined
