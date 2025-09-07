{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-} -- Keep this for Stream/VisualStream instances

module Parser where

import qualified Lexer as L
import AST
import Types
import Text.Megaparsec
import Data.Void
import Control.Monad
import Data.Maybe ( fromMaybe, isJust, fromJust )
import qualified Data.Map as Map

-- Parser monad type
type Parser = Parsec Void [L.Token]

-- Helper for chainr1 (right-associative binary operator) for Types
chainr1'Type :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1'Type p op = do
    x <- p
    go x
  where
    go x = (do
        f <- op
        y <- chainr1'Type p op -- Recursive call here
        return (f x y)
      ) <|> return x

--------------------------------------------------------------------------------
-- 1. Type Parsing
--------------------------------------------------------------------------------

-- BNF: <type> ::= <arrow_type>
-- Main entry for parsing a type
parseType :: Parser Type
parseType = parseArrowType

-- BNF: <arrow_type> ::= <basic_type> ( "->" <arrow_type> )?
-- Parses function arrow types (right-associative: A -> B -> C is A -> (B -> C))
parseArrowType :: Parser Type
parseArrowType = chainr1'Type parseBasicType (do _ <- single L.TokRightArrow; return TArr)

-- BNF: <basic_type> ::= "Int" | "String" | "Bool" | "Unit" | <list_type> | "(" <type> ")"
-- Parses atomic or parenthesized types
parseBasicType :: Parser Type
parseBasicType =
      (do L.TokIdentifier "Int"    <- satisfy (isIdentifier "Int"); return TInt)
  <|> (do L.TokIdentifier "String" <- satisfy (isIdentifier "String"); return TString)
  <|> (do L.TokIdentifier "Bool"   <- satisfy (isIdentifier "Bool"); return TBool)
  <|> (do L.TokIdentifier "Unit"   <- satisfy (isIdentifier "Unit"); return TUnit)
  <|> parseListType                -- Handles `List T` syntax
  <|> (do _ <- single L.TokLParen; t <- parseType; _ <- single L.TokRParen; return t) -- For `(A -> B)`

-- BNF: <list_type> ::= "List" <basic_type>
-- Parses list types (e.g., `List Int`)
parseListType :: Parser Type
parseListType = do
  L.TokIdentifier "List" <- satisfy $ isIdentifier "List" -- Expects the keyword "List"
  elementType <- parseBasicType -- The element type can be any basic type (or parenthesized)
  return (TList elementType)

--------------------------------------------------------------------------------
-- 2. Basic Token-to-AST Conversions
--------------------------------------------------------------------------------

-- BNF: <let_start> ::= "let" <identifier> "="
-- Parses the 'let <name> =' part of a function definition
parseLet :: Parser String
parseLet = do
  _ <- single L.TokLet
  L.TokIdentifier name <- satisfy isIdentifier'
  _ <- single L.TokEquals
  return name

parseIfElse :: Parser Expr
parseIfElse = do
  _ <- single L.TokIf
  cond <- parseExpr
  _ <- single L.TokThen
  thenBranch <- parseExpr
  _ <- single L.TokElse
  IfThenElse cond thenBranch <$> parseExpr

-- BNF: <func_arrow> ::= "=>"
-- Parses the '=>' arrow
parseArrow :: Parser ()
parseArrow = void (single L.TokArrow)

-- Helpers to check if a token is an identifier
isIdentifier :: String -> L.Token -> Bool
isIdentifier expected (L.TokIdentifier given) = given == expected
isIdentifier _ _ = False

isIdentifier' :: L.Token -> Bool
isIdentifier' (L.TokIdentifier _) = True
isIdentifier' _ = False

-- BNF: <string_literal> ::= <string_token>
-- Parses a string literal
parseStringLiteral :: Parser Expr
parseStringLiteral = do
  L.TokStringLiteral s <- satisfy (\case L.TokStringLiteral _ -> True; _ -> False)
  return (LitString s)

-- BNF: <int_literal> ::= <number_token>
-- Parses an integer literal
parseIntLiteral :: Parser Expr
parseIntLiteral = do
  L.TokNumber n <- satisfy (\case L.TokNumber _ -> True; _ -> False)
  return (LitInt n)

-- BNF: <bool_literal> ::= "True" | "False"
-- Parses a boolean literal
parseBoolLiteral :: Parser Expr
parseBoolLiteral = do
  L.TokBoolLiteral bool <- satisfy (\case L.TokBoolLiteral _ -> True; _ -> False)
  return (LitBool bool)

-- BNF: <variable> ::= <identifier>
-- Parses a variable reference
parseVariable :: Parser Expr
parseVariable = do
  L.TokIdentifier name <- satisfy isIdentifier'
  return (Var name)

-- BNF: <list_literal> ::= "[" ( <expr> ("," <expr>)* )? "]"
-- Parses a list literal (e.g., [1, 2, 3])
parseListLiteral :: Parser Expr
parseListLiteral = do
  _ <- single L.TokLBracket
  elements <- parseExpr `sepBy` single L.TokComma
  _ <- single L.TokRBracket
  return (LitList elements)

--------------------------------------------------------------------------------
-- 3. Expression Parsing with Precedence
--------------------------------------------------------------------------------

-- BNF: <expr> ::= <or_expr>
-- The main entry point for parsing any expression
parseExpr :: Parser Expr
parseExpr = parseOrExpr -- Start with the lowest precedence operator (OR)

-- Helper for chaining left-associative binary operators for Expressions
chainl1'Expr :: Parser Expr -> Parser (Expr -> Expr -> Expr) -> Parser Expr
chainl1'Expr p op = p >>= \x -> go x
  where
    go x = (do
      f <- op
      y <- p
      go (f x y)
      ) <|> return x

-- Generic binary operator parsing helper
parseBinOpToken :: (L.Token -> Maybe BinOperator) -> Parser (Expr -> Expr -> Expr)
parseBinOpToken tokenToOp = do
  opToken <- satisfy (isJust . tokenToOp)
  let op = fromJust (tokenToOp opToken)
  return (BinOp op) -- Return a function that constructs BinOp AST node

-- Token to BinOperator mappings (for parseBinOpToken)
isOrOp, isAndOp, isComparisonOp, isAdditiveOp, isMultiplicativeOp :: L.Token -> Maybe BinOperator
isOrOp L.TokPipePipe = Just Or; isOrOp _ = Nothing
isAndOp L.TokAmpAmp = Just And; isAndOp _ = Nothing
isComparisonOp L.TokEqEq = Just Eq; isComparisonOp L.TokBangEq = Just Neq; isComparisonOp L.TokLt = Just Lt; isComparisonOp L.TokGt = Just Gt; isComparisonOp L.TokLtEq = Just Le; isComparisonOp L.TokGtEq = Just Ge; isComparisonOp _ = Nothing
isAdditiveOp L.TokPlus = Just Add; isAdditiveOp L.TokMinus = Just Sub; isAdditiveOp _ = Nothing
isMultiplicativeOp L.TokStar = Just Mul; isMultiplicativeOp L.TokSlash = Just Div; isMultiplicativeOp _ = Nothing

-- Precedence levels, from lowest to highest:

-- BNF: <or_expr> ::= <and_expr> ( "||" <and_expr> )*
parseOrExpr :: Parser Expr
parseOrExpr = chainl1'Expr parseAndExpr (parseBinOpToken isOrOp)

-- BNF: <and_expr> ::= <comparison_expr> ( "&&" <comparison_expr> )*
parseAndExpr :: Parser Expr
parseAndExpr = chainl1'Expr parseComparisonExpr (parseBinOpToken isAndOp)

-- BNF: <comparison_expr> ::= <additive_expr> ( ( "==" | "!=" | "<" | ">" | "<=" | ">=" ) <additive_expr> )*
parseComparisonExpr :: Parser Expr
parseComparisonExpr = chainl1'Expr parseAdditiveExpr (parseBinOpToken isComparisonOp)

-- BNF: <additive_expr> ::= <concat_expr> ( ( "+" | "-" ) <concat_expr> )*
parseAdditiveExpr :: Parser Expr
parseAdditiveExpr = chainl1'Expr parseConcatExpr (parseBinOpToken isAdditiveOp)

-- BNF: <concat_expr> ::= <multiplicative_expr> ( "++" <multiplicative_expr> )*
parseConcatExpr :: Parser Expr
parseConcatExpr = chainl1'Expr parseMultiplicativeExpr (try (do _ <- single L.TokStrConcat; return Concat))

-- BNF: <multiplicative_expr> ::= <unary_expr> ( ( "*" | "/" ) <unary_expr> )*
parseMultiplicativeExpr :: Parser Expr
parseMultiplicativeExpr = chainl1'Expr parseUnaryExpr (parseBinOpToken isMultiplicativeOp)

-- BNF: <unary_expr> ::= "!" <unary_expr> | <term>
parseUnaryExpr :: Parser Expr
parseUnaryExpr =
      (do _ <- single L.TokBang -- Expect '!' for 'not'
          expr <- parseUnaryExpr -- Recursively parse the operand
          return (UnOp Not expr))
  <|> parseApplication -- Fall through to terms after unary

-- NEW: Function application parsing (highest precedence)
-- Parses one or more atoms, treating them as left-associative function calls.
-- e.g., `f x y` becomes `Apply (Apply (Var "f") (Var "x")) (Var "y")`
parseApplication :: Parser Expr
parseApplication = do
  exprs <- some parseAtom
  return (foldl1 Apply exprs)

-- NEW: Atomic expressions that can be applied
-- This is the new base case for the expression hierarchy.

-- Helper to check if a token is a binary operator
isBinOpToken :: L.Token -> Maybe BinOperator
isBinOpToken L.TokPlus = Just Add; isBinOpToken L.TokMinus = Just Sub
isBinOpToken L.TokStar = Just Mul; isBinOpToken L.TokSlash = Just Div
isBinOpToken L.TokAmpAmp = Just And; isBinOpToken L.TokPipePipe = Just Or
isBinOpToken L.TokEqEq = Just Eq; isBinOpToken L.TokBangEq = Just Neq
isBinOpToken L.TokLt = Just Lt; isBinOpToken L.TokGt = Just Gt
isBinOpToken L.TokLtEq = Just Le; isBinOpToken L.TokGtEq = Just Ge
isBinOpToken _ = Nothing

parseAtom :: Parser Expr
parseAtom =
      parseStringLiteral
  <|> parseBoolLiteral
  <|> parseIntLiteral
  <|> try parseIfElse
  <|> try parseListLiteral
  <|> try (do v <- parseVariable; notFollowedBy (single L.TokColon); return v)
  <|> parseDoBlock
  <|> (do -- This now handles both regular parenthesized expressions AND (op)
          _ <- single L.TokLParen
          expr <- try (do -- Operator as Function: (op)
                          op <- satisfy (isJust . isBinOpToken)
                          return $ OpAsFunction (fromJust $ isBinOpToken op)
                      )
              <|> parseExpr -- Fallback to a regular expression
          _ <- single L.TokRParen
          return expr)


--------------------------------------------------------------------------------
-- 4. Block and DoBlock Parsing
--------------------------------------------------------------------------------

-- BNF: <block> ::= "{" <expr>* "}"
parseBlock :: Parser Expr
parseBlock = do
  _ <- single L.TokLBrace
  exprs <- many parseExpr
  _ <- single L.TokRBrace
  return (Block exprs)

-- BNF: <do_block> ::= "do" "{" <expr>* "}"
parseDoBlock :: Parser Expr
parseDoBlock = do
  _ <- single L.TokDo
  _ <- single L.TokLBrace
  exprs <- many parseExpr
  _ <- single L.TokRBrace
  return (DoBlock exprs)

--------------------------------------------------------------------------------
-- 5. Function & Top-Level Declaration Parsing
--------------------------------------------------------------------------------

parseParamName :: Parser String
parseParamName = do L.TokIdentifier name <- satisfy isIdentifier'; return name

parseTypeDeclaration :: Parser (String, Type)
parseTypeDeclaration = do
  L.TokIdentifier name <- satisfy isIdentifier'
  _ <- single L.TokColon
  funcSig <- parseType
  return (name, funcSig)

parseFunctionDefinition :: Parser Function
parseFunctionDefinition = do
  name <- parseLet
  -- `many` parses ZERO or more parameters.
  -- - For `let x = 10`, it parses 0 parameters.
  -- - For `let f = a => b => ...`, it parses 2 parameters.
  params <- many (try (parseParamName <* parseArrow))
  body <- parseBlock <|> parseExpr
  effects <- optional parseRequires
  let effectList = fromMaybe [] effects
  return (Function name (TError "Type not yet assigned") params body effectList)


--------------------------------------------------------------------------------
-- 6. Effect Parsing
--------------------------------------------------------------------------------

-- BNF: <requires_clause> ::= "REQUIRES" <effect_names>
parseRequires :: Parser [Effect]
parseRequires = do
  _ <- single L.TokRequires
  parseEffectNames

-- BNF: <effect_names> ::= <effect> ("," <effect>)*
parseEffectNames :: Parser [Effect]
parseEffectNames = parseEffectName `sepBy` single L.TokComma

-- BNF: <effect> ::= "ConsoleWrite" | "FileIO" | "Network"
parseEffectName :: Parser Effect
parseEffectName = do
  L.TokIdentifier eff <- satisfy isIdentifier'
  case eff of
    "ConsoleWrite" -> return ConsoleWrite
    "FileIO"       -> return FileIO
    "Network"      -> return Network
    _             -> fail ("Unknown effect: " ++ eff)

--------------------------------------------------------------------------------
-- 7. Program Parsing (Alternative robust strategy for top-level declarations)
--------------------------------------------------------------------------------

-- A sum type to represent either a type declaration or a function definition found at the top level
data TopLevelDecl = TypeDecl (String, Type) | FuncDef Function

-- BNF: <top_level_declaration> ::= <type_declaration> | <function_definition>
-- The main program parser: collects all top-level declarations and merges them
parseTopLevelDeclaration :: Parser TopLevelDecl
parseTopLevelDeclaration =
      TypeDecl <$> try parseTypeDeclaration
  <|> FuncDef <$> parseFunctionDefinition

-- BNF: <program> ::= <top_level_declaration>* <EOF>
parseProgram :: Parser [Function]
parseProgram = do
  decls <- many parseTopLevelDeclaration -- USE MANY HERE
  -- The rest of the logic remains the same
  let typeDeclarations = [ td | TypeDecl td <- decls ]
  let functionDefinitions = [ fd | FuncDef fd <- decls ]

  let typeMap = Map.fromList typeDeclarations

  let functionsWithTypes = map (\f@Function{funcName} ->
                                  case Map.lookup funcName typeMap of
                                    Just t -> f { funcTypeSignature = t }
                                    Nothing -> error $ "Missing type declaration for function: " ++ funcName ++ ". All top-level functions must have an explicit type declaration."
                               ) functionDefinitions

  let declaredButUndefined = Map.keys $ Map.difference typeMap (Map.fromList $ map (\f -> (funcName f, ())) functionsWithTypes)
  unless (null declaredButUndefined) $
    fail $ "Error: Type declarations found for undefined functions: " ++ show declaredButUndefined

  let duplicateDefs = findDuplicates (map funcName functionDefinitions)
  unless (null duplicateDefs) $
    fail $ "Error: Duplicate function definitions for: " -- ++ show duplicateDefs

  _ <- single L.TokEOF -- This line remains at the end
  eof -- Make sure there's nothing after the EOF token
  return functionsWithTypes

-- Simple helper to find duplicates in a list (for basic error checking)
findDuplicates :: Ord a => [a] -> [a]
findDuplicates xs = Map.keys $ Map.filter (>1) $ Map.fromListWith (+) [(x, 1) | x <- xs]
