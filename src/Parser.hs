module Parser where

import qualified Lexer as L
import AST
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Control.Monad
import Data.Maybe ( fromMaybe )

type Parser = Parsec Void [L.Token]

parseLet :: Parser String
parseLet = do
  _ <- single L.TokLet
  L.TokIdentifier name <- satisfy isIdentifier
  _ <- single L.TokEquals
  return name

parseParams :: Parser [String]
parseParams = do
  _ <- single L.TokLParen
  params <- parseParamNames
  _ <- single L.TokRParen
  return params

parseParamNames :: Parser [String]
parseParamNames = parseParamName `sepBy` single L.TokComma

parseParamName :: Parser String
parseParamName = do
  L.TokIdentifier name <- satisfy isIdentifier
  return name

parseArrow :: Parser ()
parseArrow = void (single L.TokArrow)

isIdentifier :: L.Token -> Bool
isIdentifier (L.TokIdentifier _) = True
isIdentifier _ = False

parseStringLiteral :: Parser Expr
parseStringLiteral = do
  L.TokStringLiteral s <- satisfy isStringLiteral
  return (LitString s)


isStringLiteral :: L.Token -> Bool
isStringLiteral (L.TokStringLiteral _) = True
isStringLiteral _ = False

parseVariable :: Parser Expr
parseVariable = do
  L.TokIdentifier name <- satisfy isIdentifier
  return (Var name)

parseExpr :: Parser Expr
parseExpr = parseConcatExpr

parseConcatExpr :: Parser Expr
parseConcatExpr = do
  left <- parseTerm
  parseConcat left <|> return left

parseTerm :: Parser Expr
parseTerm =
      parseCall             -- e.g., greet("World")
  <|> parseStringLiteral     -- e.g., "Hello"
  <|> parseVariable
  <|> parseDoBlock
  -- <|> parseIntLiteral      -- numbers

parseConcat :: Expr -> Parser Expr
parseConcat left = do
  _ <- single L.TokStrConcat  -- parse ++ operator
  right <- parseExpr         -- parse right side recursively
  return (Concat left right)


parseBlock :: Parser Expr
parseBlock = do
  _ <- single L.TokLBrace
  exprs <- many parseExpr
  _ <- single L.TokRBrace
  return (Block exprs)

parseDoBlock :: Parser Expr
parseDoBlock = do
  _ <- single L.TokDo
  _ <- single L.TokLBrace
  exprs <- many parseExpr
  _ <- single L.TokRBrace
  return (DoBlock exprs)

parseFunction :: Parser Function
parseFunction = do
  name <- parseLet
  params <- parseParams
  parseArrow
  body <- parseBlock
  effects <- optional parseRequires
  let effectList = Data.Maybe.fromMaybe [] effects
  return (Function name params body effectList)

parseCall :: Parser Expr
parseCall = try $ do
  L.TokIdentifier fname <- satisfy isIdentifier
  _ <- single L.TokLParen  -- Must be '(' immediately after
  args <- parseExpr `sepBy` single L.TokComma
  _ <- single L.TokRParen
  return (Call fname args)


parseRequires :: Parser [Effect]
parseRequires = do
  _ <- single L.TokRequires
  parseEffectNames

parseEffectNames :: Parser [Effect]
parseEffectNames = do
  parseEffectName `sepBy` single L.TokComma

parseEffectName :: Parser Effect
parseEffectName = do
  L.TokIdentifier eff <- satisfy isIdentifier
  case eff of
    "ConsoleWrite" -> return ConsoleWrite
    "FileIO"       -> return FileIO
    "Network"      -> return Network
    _             -> fail ("Unknown effect: " ++ eff)
