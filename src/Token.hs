-- src/Token.hs
module Token where

import Data.Text (Text) -- Using Text for better string handling


-- Define all possible tokens in your Pura language
data Token = TLet             -- 'let' keyword
           | TEquals          -- '='
           | TArrow           -- '=>'
           | TOpenParen       -- '('
           | TCloseParen      -- ')'
           | TOpenBrace       -- '{'
           | TCloseBrace      -- '}'
           | TComma           -- ','
           | TPlusPlus        -- '++' (for string concatenation)
           | TRequires        -- 'REQUIRES' keyword
           | TIdentifier Text -- Variable names, function names (e.g., 'add', 'greet', 'main')
           | TString Text     -- String literals (e.g., "Hello")
           | TInt Int         -- Integer literals (e.g., 123)
           | TEffectName Text -- Effect names (e.g., 'ConsoleWrite')
           | TBuiltinPrint    -- 'print' (built-in function)
           | TBuiltinToString -- 'toString' (built-in function)
           | TEOF             -- End of File token
           deriving (Show, Eq, Ord) -- Ord for Data.Set operations later