module Types where

-- Represents types in your language
data Type
  = TInt         -- Integer type
  | TString      -- String type
  | TBool        -- Boolean type
  | TUnit        -- Represents "no meaningful value", often for side-effecting functions
  | TList Type   -- List type, parameterized by the type of its elements (e.g., List Int)
  | TArr Type Type -- Function type: argument type -> return type (for currying)
  | TError String    -- Used to signal and carry type error messages
  deriving (Show, Eq)
  