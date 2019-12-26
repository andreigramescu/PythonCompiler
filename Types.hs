module Types where

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
  | PyArithmeticExpression ArithmeticExpression
  | PyBooleanExpression BooleanExpression
  deriving (Show, Eq)

data ArithmeticExpression
  = ArithmeticValue PyValue
  | Plus ArithmeticExpression ArithmeticExpression
  | Minus ArithmeticExpression ArithmeticExpression
  | Multiply ArithmeticExpression ArithmeticExpression
  | Divide ArithmeticExpression ArithmeticExpression
  | Mod ArithmeticExpression ArithmeticExpression
  | Pow ArithmeticExpression ArithmeticExpression
  deriving (Eq)

data BooleanExpression
  = Atom PyValue
  | Compare PyValue Symbol PyValue
  -- Repositioned so that we can easily apply <$> and <*>
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

type Symbol = String

arithmOps :: [Symbol]
arithmOps
  = ["**", "%", "*", "/", "+", "-"]

booleanOps :: [Symbol]
booleanOps
  = ["not", "and", "or"]

comps :: [Symbol]
comps
  = ["<", ">", "<=", ">=", "=="]

precedences :: [(Symbol, Int)]
precedences
  =  zip (arithmOps ++ booleanOps) [3, 2, 2, 2, 1, 1,
                                    3, 2, 1]

instance Show ArithmeticExpression where
  show (ArithmeticValue v)        = show v
  show (Plus e1 e2)     = "(" ++ show e1 ++ ") + (" ++ show e2 ++ ")"
  show (Minus e1 e2)    = "(" ++ show e1 ++ ") - (" ++ show e2 ++ ")"
  show (Multiply e1 e2) = "(" ++ show e1 ++ ") * (" ++ show e2 ++ ")"
  show (Divide e1 e2)   = "(" ++ show e1 ++ ") / (" ++ show e2 ++ ")"
  show (Mod e1 e2)      = "(" ++ show e1 ++ ") % (" ++ show e2 ++ ")"
  show (Pow e1 e2)      = "(" ++ show e1 ++ ") ** (" ++ show e2 ++ ")"
