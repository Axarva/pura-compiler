module Lexer where

import Data.Char

data Token
  = TokLet
  | TokDo
  | TokEquals
  | TokLParen
  | TokRParen
  | TokLBrace
  | TokRBrace
  | TokArrow
  | TokRequires
  | TokIdentifier String
  | TokStringLiteral String
  | TokNumber Int
  | TokPlus
  | TokComma
  | TokStrConcat
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
nextToken ('=' : rest) = (TokEquals, rest)
nextToken ('+' : '+' : rest) = (TokStrConcat, rest)  -- Match ++ first
nextToken ('+' : rest) = (TokPlus, rest)            -- Then +
nextToken s@(c:_)
  | isAlpha c =
      let (ident, rest') = span isAlphaNum s
      in case ident of
          "let" -> (TokLet, rest')
          "do"  -> (TokDo, rest')
          "REQUIRES" -> (TokRequires, rest')
          other -> (TokIdentifier other, rest')
  | c == '"' =
      let (strContent, rest'') = span (/= '"') (tail s)
      in (TokStringLiteral strContent, drop 1 rest'')  -- drop closing "
  | otherwise = error $ "Unexpected character: " ++ [c]
nextToken "" = (TokEOF, "")
