module Lexer where

import Data.Char

data Token
  = TokLet
  -- Keywords and symbols
  | TokDo
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

tokenize :: String -> [Token]
tokenize "" = [TokEOF]
tokenize s =
  let (tok, rest) = nextToken (dropWhile isSpace s)
  in case tok of
       TokEOF -> [TokEOF]
       _      -> tok : tokenize rest


nextToken :: String -> (Token, String)
nextToken ('(' : rest) = (TokLParen, rest)
nextToken (')' : rest) = (TokRParen, rest)
nextToken ('{' : rest) = (TokLBrace, rest)
nextToken ('}' : rest) = (TokRBrace, rest)
nextToken ('=' : '>' : rest) = (TokArrow, rest)
nextToken ('-' : '>' : rest) = (TokRightArrow, rest)
nextToken ('-' : rest) = (TokMinus, rest)
nextToken ('*' : rest) = (TokStar, rest)
nextToken ('/' : rest) = (TokSlash, rest)
nextToken ('&' : '&' : rest) = (TokAmpAmp, rest)
nextToken ('|' : '|' : rest) = (TokPipePipe, rest)
nextToken ('!' : '=' : rest) = (TokBangEq, rest) -- For !=
nextToken ('!' : rest) = (TokBang, rest)        -- For 'not' (unary !)
nextToken ('=' : '=' : rest) = (TokEqEq, rest)  -- For ==
nextToken ('<' : '=' : rest) = (TokLtEq, rest)
nextToken ('<' : rest) = (TokLt, rest)
nextToken ('>' : '=' : rest) = (TokGtEq, rest)
nextToken ('>' : rest) = (TokGt, rest)
nextToken ('=' : rest) = (TokEquals, rest)
nextToken ('+' : '+' : rest) = (TokStrConcat, rest)  -- Match ++ first
nextToken ('+' : rest) = (TokPlus, rest)            -- Then +
nextToken (':' : rest) = (TokColon, rest)
nextToken ('[' : rest) = (TokLBracket, rest)
nextToken (']' : rest) = (TokRBracket, rest)
nextToken s@(c:_)
  | isAlpha c =
      let (ident, rest') = span isAlphaNum s
      in case ident of
          "let" -> (TokLet, rest')
          "do"  -> (TokDo, rest')
          "REQUIRES" -> (TokRequires, rest')
          "True" -> (TokBoolLiteral True, rest')
          "False" -> (TokBoolLiteral False, rest')
          other -> (TokIdentifier other, rest')
  | c == '"' =
      let (strContent, rest'') = span (/= '"') (tail s)
      in (TokStringLiteral strContent, drop 1 rest'')  -- drop closing "
  | otherwise = error $ "Unexpected character: " ++ [c]
nextToken "" = (TokEOF, "")
