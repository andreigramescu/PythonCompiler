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
  | PyArithmExpr ArithmExpr
  | PyBoolExpr BoolExpr
  deriving (Show, Eq)

data ArithmExpr
  = ArithmVal PyValue
  | Plus ArithmExpr ArithmExpr
  | Minus ArithmExpr ArithmExpr
  | Mul ArithmExpr ArithmExpr
  | Div ArithmExpr ArithmExpr
  | Mod ArithmExpr ArithmExpr
  | Pow ArithmExpr ArithmExpr
  deriving (Eq)

data BoolExpr
  = Atom PyValue
  | Comp PyValue Symbol PyValue
  | Not BoolExpr
  | And BoolExpr BoolExpr
  | Or BoolExpr BoolExpr
  deriving (Eq)

data Procedure
  = Assignment Name PyValue
  | If BoolExpr [Procedure]
  | While BoolExpr [Procedure]
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

instance Show ArithmExpr where
  show (ArithmVal v) = show v
  show (Plus e1 e2)  = "(" ++ show e1 ++ ") + (" ++ show e2 ++ ")"
  show (Minus e1 e2) = "(" ++ show e1 ++ ") - (" ++ show e2 ++ ")"
  show (Mul e1 e2)   = "(" ++ show e1 ++ ") * (" ++ show e2 ++ ")"
  show (Div e1 e2)   = "(" ++ show e1 ++ ") / (" ++ show e2 ++ ")"
  show (Mod e1 e2)   = "(" ++ show e1 ++ ") % (" ++ show e2 ++ ")"
  show (Pow e1 e2)   = "(" ++ show e1 ++ ") ** (" ++ show e2 ++ ")"

instance Show BoolExpr where
  show (Atom a) = show a
  show (Comp e1 cmp e2)  = "(" ++ show e1 ++ ")" ++
                          show cmp ++ "(" ++ show e2 ++ ")"
  show (Not e)  = "-" ++ "(" ++ show e ++ ")"
  show (And e1 e2) = "(" ++ show e1 ++ ") & (" ++ show e2 ++ ")"
  show (Or e1 e2)  = "(" ++ show e1 ++ ") | (" ++ show e2 ++ ")"
