module AST where

import Types

data Expr
  = Var String
  | LitInt Int
  | LitString String
  | LitBool Bool
  | LitList [Expr]
  | Concat Expr Expr
  | Call String [Expr]
  | BinOp BinOperator Expr Expr -- New: For binary operators (+, -, *, /, &&, ||, ==, !=, <, >, <=, >=)
  | UnOp UnOperator Expr      -- New: For unary operators (not)
  | Block [Expr]
  | DoBlock [Expr]
  deriving (Show, Eq)

data BinOperator
  = Add | Sub | Mul | Div
  | And | Or
  | Eq | Neq | Lt | Gt | Le | Ge
  deriving (Show, Eq)

data UnOperator
  = Not
  deriving (Show, Eq)

data Function = Function
  { funcName        :: String
  , funcTypeSignature :: Type
  , funcArgs        :: [String]
  , funcBody        :: Expr
  , funcEffects     :: [Effect]
  } deriving (Show, Eq)


data Effect = ConsoleWrite | FileIO | Network deriving (Show, Eq)
