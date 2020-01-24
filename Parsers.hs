module Parsers where

import           Types
import           Control.Applicative
import           Data.Char
import           Data.Maybe

{-newtype Parser a
  = Parser {
  runParser :: String -> Maybe (String, a)
  }
-}

newtype Parser m a
  = Parser {
  runParser :: String -> m (String, a)
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

--STATET

instance Monad Parser where
  return x = Parser (\input -> Just (input, x))
  p >>= f = Parser (\input -> do
                      (extra, val) <- runParser p input
                      (extra', val') <- runParser (f val) extra
                      return (extra', val'))

charP :: Char -> Parser Char
charP c = satisfyP (== c)

-- Helper parsers
satisfyP :: (Char -> Bool) -> Parser Char
satisfyP f = itemP >?> f

itemP :: Parser Char
itemP = Parser f
  where
    f "" = empty
    f (c:cs) = pure (cs, c)

class Applicative f => Selective f where
  branch :: f (Either a b) -> f (a -> c) -> f (b -> c) -> f c

instance Selective Parser where
  branch b p q = b >>= (\x -> case x of
    Left x -> p <*> pure x
    Right y -> q <*> pure y)

select :: Selective f => f (Either a b) -> f (a -> b) -> f b
select p q = branch p q (pure id)

(>?>) :: Parser a -> (a -> Bool) -> Parser a
p >?> f = select (g <$> p) empty
  where
    g x 
      | f x = Right x
      | otherwise = Left ()
      

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

removeBracketsP :: Parser a -> Parser a
removeBracketsP p
  = p
    <|> bracketsP (removeBracketsP p)
    {-<|> bracketsP (bracketsP p)
    <|> bracketsP (bracketsP (bracketsP p))
    <|> bracketsP (bracketsP (bracketsP (bracketsP p)))
    <|> bracketsP (bracketsP (bracketsP (bracketsP (bracketsP p))))
    <|> bracketsP (bracketsP (bracketsP (bracketsP (bracketsP (bracketsP p)))))
    <|> bracketsP (bracketsP (bracketsP (bracketsP (bracketsP (bracketsP (bracketsP p))))))-}
    where
      bracketsP p = charP '(' *> whiteSpaceP *> p <* whiteSpaceP <* charP ')'

-- Actual parsing

-- Parsing values and objects
pyNone' :: Parser PyValue
pyNone'
  = PyNone <$ stringP "None"

pyBool' :: Parser PyValue
pyBool'
  = PyBool True <$ stringP "True" <|> PyBool False <$ stringP "False"

(<:>) :: Parser a -> Parser [a] -> Parser [a]
(<:>) = liftA2 (:)

pyInt' :: Parser PyValue
pyInt'
  = f <$> ( nextDigitsP <|>
           charP '-' <:> nextDigitsP)
  where
    f = PyInt . read
    nextDigitsP = fromNullParsedP (spanP isDigit)

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
  = PyList <$> (charP '[' *> whiteSpaceP *> elements <* whiteSpaceP <* charP ']')
  where
    elements = ((:) <$> (pyValue <* whiteSpaceP) <*> many (charP ',' *> whiteSpaceP *> pyValue <* whiteSpaceP))
                <|> pure []


pyVariable' :: Parser PyValue
pyVariable'
  = PyVariable <$> fromNullParsedP (Parser (\input -> do
                                            (extra, name) <- runParser (spanP isAlpha) input
                                            if name == "or" || name == "and" || name == "not"
                                            then return (input, "")
                                            else return (extra, name)))

pyDict' :: Parser PyValue
pyDict'
  = PyDict <$> (charP '{' *> whiteSpaceP *> bindings <* whiteSpaceP <* charP '}')
    where
      bindings = ((:) <$> (pyPair <* whiteSpaceP) <*> many (charP ',' *> whiteSpaceP *> pyPair <* whiteSpaceP)) <|> pure []
      pyPair
        = Parser (\input -> do
            (extra, key) <- runParser pyValue input
            (extra', val) <- runParser (whiteSpaceP *> charP ':' *> whiteSpaceP *> pyValue) extra
            return (extra', (key, val)))

pyFunctionCall' :: Parser PyValue
pyFunctionCall'
  = PyFunctionCall <$>
    fromNullParsedP (spanP isAlpha) <*>
    (whiteSpaceP *> charP '(' *> whiteSpaceP *> arguments <* whiteSpaceP <* charP ')')
    where
      arguments = ((:) <$> (pyValue <* whiteSpaceP) <*> many (charP ',' *> whiteSpaceP *> pyValue <* whiteSpaceP))
                  <|> pure []

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
    <|> (PyArithmeticExpression <$> pyArithmeticExpression)
    <|> (PyBooleanExpression <$> pyBooleanExpression)

--Parsing arithmetic expressions
pyArithmeticValue :: Parser ArithmeticExpression
pyArithmeticValue
  = ArithmeticValue <$> (pyInt <|> pyFunctionCall <|> pyVariable)

{-data Ops a = LeftAssoc (Parser (a -> a -> a))
           | RightAssoc (Parser (a -> a -> a))

prec :: Parser a -> [Ops a] -> Parser a
prec atom ops = foldl atom (map convert ops)
  where
    convert (LeftAssoc op) p = chainl1 p op
    convert (RightAssoc op) p = chainr1 p op

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = flip (foldl ($)) <$> p <*> many (flip <$> op <*> p)

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op = foldr (flip ($)) <$> many (p <**> op) <*> p

expr = prec number [LeftAssoc ((*) <$ char '*'),
                    LeftAssoc ((-) <$ char '-'),
                    LeftAssoc ((+) <$ char '+')] -}

pyArithmeticExpression :: Parser ArithmeticExpression
pyArithmeticExpression
  = Parser (\input -> do
      (extra, expr) <- runParser (many ((opP <|> valP) <* whiteSpaceP)) input
      if expr == []
      then Nothing
      else return (extra, (fst . convertBack . reverse . postfix) expr))
  where
    opP = Left <$> (stringP "(" <|> stringP ")" <|> foldl1 (<|>) (map stringP arithmOps))
    valP = Right <$> pyArithmeticValue
    postfix :: [Either Symbol ArithmeticExpression] -> [Either Symbol ArithmeticExpression]
    postfix = postfix' []
      where
        postfix' stack [] = map Left stack
        postfix' stack ((Left x) : expr')
          | x == "("  = postfix' (x : stack) expr'
          | x == ")"  = map Left opsInBrackets ++ postfix' newStack expr'
          | otherwise = map Left higherPrecedence ++ postfix' newStack' expr'
          where
            (opsInBrackets, _ : newStack) = span (/="(") stack
            (higherPrecedence, newStack'') = span (\y -> y /= "(" && lookUp y precedences >= lookUp x precedences) stack
            newStack' = x : newStack''
        postfix' stack ((Right x) : expr')
          = Right x : postfix' stack expr'
    -- From reversed postfix to the needed expression of type ArithmeticExpression
    convertBack :: [Either Symbol ArithmeticExpression] -> (ArithmeticExpression, [Either Symbol ArithmeticExpression])
    convertBack [] = error "A case yet to be solved!"
    convertBack (Left x : expr') = ((lookUp x equivTable) nextExpression' nextExpression, extra)
      where
        equivTable = zip arithmOps [Pow, Mod, Multiply, Divide, Plus, Minus]
        (nextExpression, extra') = convertBack expr'
        (nextExpression', extra) = convertBack extra'
    convertBack (Right x : expr') = (x, expr')
    -- Helper lookup
    lookUp :: Eq a => a -> [(a, b)] -> b
    lookUp
      = ((.) . (.)) fromJust lookup

-- Parsing boolean expressions
pyAtom :: Parser BooleanExpression
pyAtom
  = Atom <$> pyValue  -- It is for the interpreting stage to decide if it is boolean

-- Pretty gross I know. Feel free to manipulate the parsers without input.
-- That would be more concise but I dont immediately see how
-- pyNot :: Parser BooleanExpression
-- pyNot
--  = Parser (\input -> do
--    (extra, _)          <- runParser (stringP "not") input
--    (extra', _)         <- runParser (charP ' ') extra
--    (extra'', _)        <- runParser whiteSpaceP extra'
--    (extra''', boolExp) <- runParser pyBooleanExpression extra''
--    return (extra''', Not boolExp)
--    )
-- could be just stringP "not " *> whiteSpaceP *> pyBooleanExpression ?

pyPurelyBooleanExpression :: Parser BooleanExpression
-- This will be similar to the ArithmeticExpression parser
-- and will parse pure boolean expressions that have not and or operators
pyPurelyBooleanExpression
  = undefined

pyCompare :: Parser BooleanExpression
-- Will leave it with pyValues, for now
-- I am pretty sure we will be changing this in the future
pyCompare
  = Compare <$> pyValue <* whiteSpaceP <*> foldl1 (<|>) (map stringP comps) <* whiteSpaceP <*> pyValue

pyBooleanExpression :: Parser BooleanExpression
pyBooleanExpression
  = pyPurelyBooleanExpression <|> pyCompare

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
