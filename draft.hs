import Types
import Parsers
import Control.Applicative

fromInfix :: Parser a -> Parser b -> Parser c -> (a -> b -> d) -> Parser d
fromInfix t1P t2P op build
  = do
      t1 <- t1P
      whiteSpaceP
      op
      whiteSpaceP
      t2 <- t2P
      return (build t1 t2)

expr :: Parser ArithmeticExpression
expr = fromRA <$> (fromInfix term expr (charP '+') Plus
        <|> fromInfix term expr (charP '-') Minus)
        <|> term
      where
        fromRA (Plus a (Plus b c)) = Plus (Plus a b) c
        fromRA (Plus a (Minus b c)) = Minus (Plus a b) c
        fromRA (Minus a (Plus b c)) = Plus (Minus a b) c
        fromRA (Minus a (Minus b c)) = Minus (Minus a b) c
        fromRA x = x

term :: Parser ArithmeticExpression
term = fromRA <$> (fromInfix factor term (charP '*') Multiply
        <|> fromInfix factor term (charP '/') Divide)
        <|> factor
      where
        fromRA (Multiply a (Multiply b c)) = Multiply (Multiply a b) c
        fromRA (Multiply a (Divide b c)) = Divide (Multiply a b) c
        fromRA (Divide a (Multiply b c)) = Multiply (Divide a b) c
        fromRA (Divide a (Divide b c)) = Divide (Divide a b) c
        fromRA x = x

factor :: Parser ArithmeticExpression
factor = fromInfix base factor (stringP "**") Pow
        <|> base

base :: Parser ArithmeticExpression
base = charP '(' *> whiteSpaceP *> expr <* whiteSpaceP <* charP ')'
        <|> ArithmeticValue <$> pyInt
