module Lexer where

import Data.Char
import Data.List (isPrefixOf)

data Token
  = TokLet
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
tokenize s =
  let stripped = strip s
  in if null stripped
       then [TokEOF]
       else let (tok, rest) = nextToken stripped
            in tok : tokenize rest

strip :: String -> String
strip s =
  let afterSpace = dropWhile isSpace s
  in if "--" `isPrefixOf` afterSpace
       -- If we find a comment, drop the whole line and try stripping again
       then strip (dropWhile (/= '\n') afterSpace)
       -- Otherwise, the string is clean and ready for the next token
       else afterSpace


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
nextToken (',' : rest) = (TokComma, rest)
nextToken s@(c:_)
  | isNumber c =
    let (ident, rest') = span isNumber s
    in case ident of x -> (TokNumber (read x :: Int), rest')
  | isAlpha c =
      let (ident, rest') = span isAlphaNum s
      in case ident of
          "let" -> (TokLet, rest')
          "if"  -> (TokIf, rest')
          "then" -> (TokThen, rest')
          "else" -> (TokElse, rest')
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
