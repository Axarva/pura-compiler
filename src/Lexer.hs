{-# LANGUAGE FlexibleInstances #-}

module Lexer where

import Data.Char
import Data.List (isPrefixOf)
import qualified Text.Megaparsec as TM
import qualified Data.List.NonEmpty as NE


data Token = Token
  { tokType :: TokenType
  , tokLine :: Int
  , tokCol  :: Int
  } deriving (Show, Eq, Ord)

data TokenType
  = TokLet
  | TokIn -- Pura will finally have nested let!
  -- Keywords and symbols
  | TokDo
  | TokIf
  | TokThen
  | TokElse
  | TokEquals
  | TokLParen
  | TokRParen
  | TokLBrace
  | TokRBrace
  | TokArrow
  | TokRightArrow
  | TokRequires
  | TokComma
  | TokMinus
  | TokStar
  | TokSlash
  | TokColon
  | TokSemicolon
  | TokAmpAmp    -- (for &&)
  | TokPipePipe  -- (for ||)
  | TokBang      -- (for !)
  | TokEqEq      -- (for ==)
  | TokBangEq    -- (for !=)
  | TokLt        -- (for <)
  | TokGt        -- (for >)
  | TokLtEq      -- (for <=)
  | TokGtEq      -- (for >=)
  | TokLBracket
  | TokRBracket
  -- Strings
  | TokIdentifier String
  | TokStringLiteral String
  | TokStrConcat
  --Numbers
  | TokNumber Int
  | TokPlus
  --Booleans
  | TokBoolLiteral Bool
  --EOF
  | TokEOF
  deriving (Show, Eq, Ord)

instance TM.VisualStream [Token] where
  showTokens _ ts = unwords $ map showToken (NE.toList ts)
    where
      showToken (Token tt line col) =
        show tt ++ "@" ++ show line ++ ":" ++ show col

  tokensLength _ = NE.length

instance TM.TraversableStream [Token] where
  reachOffset _ posState = (Nothing, posState)

tokenize :: String -> [Token]
tokenize source = tokenizeWithPos source 1 1
  where
    -- tokenizeWithPos processes the source while tracking line and column
    tokenizeWithPos :: String -> Int -> Int -> [Token]
    tokenizeWithPos s line col =
      let stripped = stripWhitespace s line col
      in case stripped of
          ([], _, _) -> [Token TokEOF line col]
          (rest, newLine, newCol) ->
            let (tok, remaining, finalLine, finalCol) = nextToken rest newLine newCol
            in tok : tokenizeWithPos remaining finalLine finalCol


stripWhitespace :: String -> Int -> Int -> (String, Int, Int)
stripWhitespace [] line col = ([], line, col)
stripWhitespace s@(c:cs) line col
  | isSpace c =
      if c == '\n'
        then stripWhitespace cs (line + 1) 1  -- New line: increment line, reset column
        else stripWhitespace cs line (col + 1) -- Other space: just increment column
  | "--" `isPrefixOf` s =
      -- Comment: skip until newline
      let (_, afterComment) = span (/= '\n') s
      in stripWhitespace afterComment line col
  | otherwise = (s, line, col)  -- No more whitespace, return position


-- Extract one token and return (Token, remaining string, new line, new column)
nextToken :: String -> Int -> Int -> (Token, String, Int, Int)
nextToken [] line col = (Token TokEOF line col, [], line, col)

-- Single character tokens
nextToken ('(' : rest) line col = (Token TokLParen line col, rest, line, col + 1)
nextToken (')' : rest) line col = (Token TokRParen line col, rest, line, col + 1)
nextToken ('{' : rest) line col = (Token TokLBrace line col, rest, line, col + 1)
nextToken ('}' : rest) line col = (Token TokRBrace line col, rest, line, col + 1)
nextToken ('[' : rest) line col = (Token TokLBracket line col, rest, line, col + 1)
nextToken (']' : rest) line col = (Token TokRBracket line col, rest, line, col + 1)
nextToken (',' : rest) line col = (Token TokComma line col, rest, line, col + 1)
nextToken (':' : rest) line col = (Token TokColon line col, rest, line, col + 1)
nextToken (';' : rest) line col = (Token TokSemicolon line col, rest, line, col + 1)
nextToken ('*' : rest) line col = (Token TokStar line col, rest, line, col + 1)
nextToken ('/' : rest) line col = (Token TokSlash line col, rest, line, col + 1)

-- Two character tokens (must check these before single-char versions)
nextToken ('=' : '>' : rest) line col = (Token TokArrow line col, rest, line, col + 2)
nextToken ('-' : '>' : rest) line col = (Token TokRightArrow line col, rest, line, col + 2)
nextToken ('&' : '&' : rest) line col = (Token TokAmpAmp line col, rest, line, col + 2)
nextToken ('|' : '|' : rest) line col = (Token TokPipePipe line col, rest, line, col + 2)
nextToken ('!' : '=' : rest) line col = (Token TokBangEq line col, rest, line, col + 2)
nextToken ('=' : '=' : rest) line col = (Token TokEqEq line col, rest, line, col + 2)
nextToken ('<' : '=' : rest) line col = (Token TokLtEq line col, rest, line, col + 2)
nextToken ('>' : '=' : rest) line col = (Token TokGtEq line col, rest, line, col + 2)
nextToken ('+' : '+' : rest) line col = (Token TokStrConcat line col, rest, line, col + 2)

-- Single char tokens that need to come AFTER two-char checks
nextToken ('-' : rest) line col = (Token TokMinus line col, rest, line, col + 1)
nextToken ('!' : rest) line col = (Token TokBang line col, rest, line, col + 1)
nextToken ('=' : rest) line col = (Token TokEquals line col, rest, line, col + 1)
nextToken ('<' : rest) line col = (Token TokLt line col, rest, line, col + 1)
nextToken ('>' : rest) line col = (Token TokGt line col, rest, line, col + 1)
nextToken ('+' : rest) line col = (Token TokPlus line col, rest, line, col + 1)

-- Numbers: consume all digits
nextToken s@(c:_) line col
  | isDigit c =
      let (digits, rest) = span isDigit s
          num = read digits :: Int
          len = length digits
      in (Token (TokNumber num) line col, rest, line, col + len)

-- Identifiers and keywords
nextToken s@(c:_) line col
  | isAlpha c =
      let (ident, rest) = span isAlphaNum s
          len = length ident
          tokType = case ident of
            "let"      -> TokLet
            "in"       -> TokIn
            "if"       -> TokIf
            "then"     -> TokThen
            "else"     -> TokElse
            "do"       -> TokDo
            "REQUIRES" -> TokRequires
            "True"     -> TokBoolLiteral True
            "False"    -> TokBoolLiteral False
            _          -> TokIdentifier ident
      in (Token tokType line col, rest, line, col + len)

-- String literals
nextToken ('"' : rest) line col =
    let 
      parseStringContent [] = error $ "Unterminated string starting at line " ++ show line
      
      -- 1. Closing Quote: We are done.
      parseStringContent ('"' : xs) = ("", xs)
      
      -- 2. Escape Sequences: We consume the backslash and produce the REAL char.
      parseStringContent ('\\' : 'n' : xs)  = consChar '\n' xs
      parseStringContent ('\\' : 't' : xs)  = consChar '\t' xs
      parseStringContent ('\\' : 'r' : xs)  = consChar '\r' xs
      parseStringContent ('\\' : '"' : xs)  = consChar '"'  xs
      parseStringContent ('\\' : '\\' : xs) = consChar '\\' xs
      
      -- 3. Any other escape (e.g. \'): Just return the character, DROP the backslash.
      -- This solves your single quote issue! \' becomes '
      parseStringContent ('\\' : c : xs)    = consChar c xs
      
      -- 4. Normal characters
      parseStringContent (x : xs) = consChar x xs

      -- Helper to chain the recursive calls
      consChar c list = 
          let (str, rem) = parseStringContent list
          in (c : str, rem)

      (strContent, remaining) = parseStringContent rest
      -- Calculate rough length for column update (not perfect for tabs, but fine for error reporting)
      len = length strContent + 2 
    in 
      (Token (TokStringLiteral strContent) line col, remaining, line, col + len)

-- Unexpected character
nextToken (c:_) line col =
  error $ "Unexpected character '" ++ [c] ++ "' at line " ++ show line ++ ", column " ++ show col
