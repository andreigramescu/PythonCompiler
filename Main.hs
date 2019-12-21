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

-- We will have to settle on parsing a single pair of brackets for now
-- since it is really really hard to parse (((((((*something*))))))).
-- The commented solutions works but only when *something* does not start
-- or end with '(' or ')'. ((((a + b) * c))) cannot be parsed, for instance.
-- If you have any ideas please do modify this function.
removeBracketsP :: Parser a -> Parser a
removeBracketsP p
  = p <|> (stringP "(" *> whiteSpaceP *> p <* whiteSpaceP <* stringP ")")
  -- = p <|> Parser f
  -- where
  --   f input
  --     = do
  --         (input', firstBrackets) <- runParser (many (charP '(' <* whiteSpaceP)) input
  --         (input'', valParsed)    <- runParser p input'
  --         (input''', lastBrackets) <- runParser (many (whiteSpaceP *> charP ')')) input''
  --         if length firstBrackets == length lastBrackets
  --         then return (input''', valParsed)
  --         else Nothing

-- Actual parsing

-- Parsing values and objects
pyNone' :: Parser PyValue
pyNone'
  = PyNone <$ stringP "None"

pyBool' :: Parser PyValue
pyBool'
  = f <$> (stringP "True" <|> stringP "False")
  where
    f "True"  = PyBool True
    f "False" = PyBool False

pyInt' :: Parser PyValue
pyInt'
  = f <$> ( nextDigitsP <|>
           (:) <$> charP '-' <*> nextDigitsP)
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
  = PyVariable <$> fromNullParsedP (spanP isAlpha)

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

--Parsing arithmetic expressions
pyArithmeticValue :: Parser ArithmeticExpression
pyArithmeticValue
  = ArithmeticValue <$> p
  where
    p = pyInt <|> pyFunctionCall <|> pyVariable

pyArithmeticOp :: Char -> Parser ArithmeticExpression
pyArithmeticOp opChar
  = (opCon <$> pyArithmeticValue
          <*> (whiteSpaceP *> charP opChar *> whiteSpaceP *> pyArithmeticExpression))
          <|>
    (opCon <$> (charP '(' *> whiteSpaceP *> pyArithmeticExpression <* whiteSpaceP <* charP ')')
          <*> (whiteSpaceP *> charP opChar *> whiteSpaceP *> pyArithmeticExpression))
  where
    ops =[('+', Plus), ('-', Minus), ('*', Multiply), ('/', Divide), ('%', Mod)]
    opCon
      = fromJust $ lookup opChar ops

pyPow :: Parser ArithmeticExpression
pyPow
  = (Pow <$> pyArithmeticValue
          <*> (whiteSpaceP *> charP '*' *> charP '*' *> whiteSpaceP *> pyArithmeticExpression))
          <|>
    (Pow <$> (charP '(' *> whiteSpaceP *> pyArithmeticExpression <* whiteSpaceP <* charP ')')
          <*> (whiteSpaceP *> charP '*' *> charP '*' *> whiteSpaceP *> pyArithmeticExpression))

pyArithmeticExpression :: Parser ArithmeticExpression
pyArithmeticExpression
  = pyArithmeticOp '+' <|> pyArithmeticOp '-' <|> pyArithmeticOp '*'
  <|> pyArithmeticOp '/' <|> pyArithmeticOp '%' <|> pyPow <|> pyArithmeticValue

-- Parsing boolean expressions
pyAtom :: Parser BooleanExpression
pyAtom
  = Atom <$> pyValue  -- It is for the interpreting stage to decide if it is boolean

pyCompare :: Parser BooleanExpression
pyCompare
  = undefined

-- I dont think we need BooleanFunctionCall. This is covered under a normal pyValue
-- pyBooleanFunctionCall :: Parser BooleanExpression
-- pyBooleanFunctionCall
--   = undefined

pyNot :: Parser BooleanExpression
pyNot
  -- Pretty gross I know. Feel free to manipulate the parsers without input.
  -- That would be more concise but I dont immediately see how
  = Parser (\input -> do
      (extra, _)          <- runParser (stringP "not") input
      (extra', _)         <- runParser (charP ' ') extra
      (extra'', _)        <- runParser whiteSpaceP extra'
      (extra''', boolExp) <- runParser pyBooleanExpression extra''
      return (extra''', Not boolExp)
  )

pyAnd :: Parser BooleanExpression
pyAnd
  = undefined

pyOr :: Parser BooleanExpression
pyOr
  = undefined

pyBooleanExpression :: Parser BooleanExpression
pyBooleanExpression
  = pyAtom <|> pyNot

-- Parsing procedures
pyAssignment :: Parser Procedure
pyAssignment
  = Parser (\input -> do
      (extra, varName) <- runParser (spanP isAlpha) input
      (extra', varValue) <- runParser (whiteSpaceP *> charP '=' *> whiteSpaceP *> pyValue) extra
      -- NOTE: here is the redundant code; we have varValue inside both the PyVariable and the Assignment
      return (extra', Assignment varName varValue)
      )

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
