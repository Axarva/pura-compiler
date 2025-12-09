module AST where

import Types

data Expr
  = Var String
  | LitInt Int
  | LitString String
  | LitBool Bool
  | LitList [Expr]
  | LitUnit
  | Concat Expr Expr
  | Apply Expr Expr
  | BinOp BinOperator Expr Expr -- For binary operators (+, -, *, /, &&, ||, ==, !=, <, >, <=, >=)
  | UnOp UnOperator Expr      -- For unary operators (not)
  | Block [Expr]
  | DoBlock [Expr]
  | Let String Expr Expr      -- Local let bindings!!!
    -- Treat a binary operator itself as a value, e.g. (+)
  | OpAsFunction BinOperator
  | IfThenElse Expr Expr Expr -- If-else statements finally
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


data Effect = ConsoleWrite | FileIO | Network | BrowserPrompt deriving (Show, Eq)
