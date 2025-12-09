module Types where

import qualified Data.Map as Map

-- Represents types in your language
data Type
  = TInt         -- Integer type
  | TString      -- String type
  | TBool        -- Boolean type
  | TUnit        -- Represents "no meaningful value", often for side-effecting functions
  | TList Type   -- List type, parameterized by the type of its elements (e.g., List Int)
  | TArr Type Type -- Function type: argument type -> return type (for currying)
  | TError String    -- Used to signal and carry type error messages
  | TAttribute Type   -- Attributes now carry types and are polymporphic for robust propagation between components
  | THtml Type   -- ADDED 2025/10/15: Represents an HTML node, parameterized by the message type (e.g., Html Msg)
  | TMsg
  | TVar String  -- ADDED 2025/11/19: For HM type inference
  deriving (Show, Eq, Ord)
  
-- Type Scheme
-- Represents a type with quantified variables.
-- e.g., "forall a. a -> a" is Scheme ["a"] (TArr (TVar "a") (TVar "a"))
data Scheme = Forall [String] Type
  deriving (Show, Eq)

-- A substitution is a mapping from variable names to types
type Subst = Map.Map String Type

-- Global environment for function names and their schemes
type GlobalEnv = Map.Map String Scheme

-- Local environment for function parameters and local bindings
-- Now holds schemes just like GlobalEnv since we have let polymorphism
type TypeEnv = Map.Map String Scheme