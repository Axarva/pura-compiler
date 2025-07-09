-- src/AST.hs
module AST where

import Data.Text (Text)
import Data.Set (Set)

-- Define the types of effects our language supports
data Effect = ConsoleWrite
            | RandomGen     -- Example of another effect you might add later
            | FilesystemIO  -- Example of another effect
            deriving (Show, Eq, Ord) -- Ord for Set operations in EffectChecker

-- Define binary operators (for expressions)
data BinOp = Add          -- for x + y
           | Subtract     -- for x - y
           | Concat       -- for s1 ++ s2
           deriving (Show, Eq)

-- Define built-in operations that inherently cause effects
data EffectOp = PrintOp -- For the 'print(message)' built-in
              deriving (Show, Eq)

-- The core expression type
data Expr = EInt Int
          | EString Text
          | EVar Text             -- Variable reference (e.g., 'x', 'message')
          | EApp Text [Expr]      -- Function application (e.g., 'greet("World")')
          | EBinOp BinOp Expr Expr -- Binary operations
          | EEffectOp EffectOp Expr -- Built-in effectful operation (e.g., 'print(message)')
          | EBlock [Expr]         -- A block of expressions (for 'main = { ... }')
          deriving (Show, Eq)

-- Top-level declarations in an Pura program
data Decl = FuncDecl { declName :: Text
                     , declParams :: [Text]
                     , declBody :: Expr
                     , declEffects :: Set Effect -- Declared effects for the function
                     }
          | MainDecl { mainBody :: Expr
                     , mainEffects :: Set Effect -- Declared effects for 'main'
                     }
          deriving (Show, Eq)

-- The complete Pura program structure
data Program = Program { programDecls :: [Decl] }
             deriving (Show, Eq)