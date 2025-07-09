module AST where

data Expr
  = Var String
  | LitInt Int
  | LitString String
  | Add Expr Expr
  | Concat Expr Expr
  | Call String [Expr]
  | Block [Expr]
  | DoBlock [Expr]
  deriving (Show, Eq)

data Function = Function
  { funcName      :: String
  , funcArgs      :: [String]
  , funcBody      :: Expr
  , funcEffects   :: [Effect]
  } deriving (Show, Eq)

data Effect = ConsoleWrite | FileIO | Network deriving (Show, Eq)
