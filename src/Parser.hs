{-# LANGUAGE NamedFieldPuns #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE TypeFamilies #-}

module Parser where

import qualified Lexer as L
import AST
import Types
import Text.Megaparsec hiding (tokens)
import Data.Void
import Control.Monad
import Data.Maybe ( fromMaybe, isJust, fromJust )
import qualified Data.Map as Map
-- import Debug.Trace (traceShow)


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

matchTok :: L.TokenType -> Parser ()
matchTok expected = do
  t <- satisfy (\tok -> L.tokType tok == expected)
  return ()

-- Helper to get the TokenType from a Token
getTokType :: L.Token -> L.TokenType
getTokType = L.tokType

-- Extract an identifier's name
identifier :: Parser String
identifier = do
  t <- satisfy (\tok -> case L.tokType tok of
                         L.TokIdentifier _ -> True
                         _ -> False)
  case L.tokType t of
    L.TokIdentifier name -> return name
    _ -> fail "Expected identifier"  -- Should never happen

-- Extract a number
number :: Parser Int
number = do
  t <- satisfy (\tok -> case L.tokType tok of
                         L.TokNumber _ -> True
                         _ -> False)
  case L.tokType t of
    L.TokNumber n -> return n
    _ -> fail "Expected number"

-- Extract a string literal
stringLit :: Parser String
stringLit = do
  t <- satisfy (\tok -> case L.tokType tok of
                         L.TokStringLiteral _ -> True
                         _ -> False)
  case L.tokType t of
    L.TokStringLiteral s -> return s
    _ -> fail "Expected string literal"

-- Extract a boolean literal
boolLit :: Parser Bool
boolLit = do
  t <- satisfy (\tok -> case L.tokType tok of
                         L.TokBoolLiteral _ -> True
                         _ -> False)
  case L.tokType t of
    L.TokBoolLiteral b -> return b
    _ -> fail "Expected boolean literal"

--------------------------------------------------------------------------------
-- 1. Type Parsing
--------------------------------------------------------------------------------

-- BNF: <type> ::= <arrow_type>
-- Main entry for parsing a type
parseType :: Parser Type
parseType = parseArrowType

-- HTML EXTRA TYPES
parseHtmlType :: Parser Type
parseHtmlType = do
  _ <- satisfy $ isIdentifier "Html"
  THtml <$> parseBasicType

-- BNF: <arrow_type> ::= <basic_type> ( "->" <arrow_type> )?
-- Parses function arrow types (right-associative: A -> B -> C is A -> (B -> C))
parseArrowType :: Parser Type
parseArrowType = chainr1'Type parseBasicType (do _ <- matchTok L.TokRightArrow; return TArr)

-- BNF: <basic_type> ::= "Int" | "String" | "Bool" | "Unit" | <list_type> | "(" <type> ")"
-- Parses atomic or parenthesized types
parseBasicType :: Parser Type
parseBasicType =
      (do _ <- satisfy (isIdentifier "Int"); return TInt)
  <|> (do _ <- satisfy (isIdentifier "String"); return TString)
  <|> (do _ <- satisfy (isIdentifier "Bool"); return TBool)
  <|> (do _ <- satisfy (isIdentifier "Unit"); return TUnit)
  <|> (do _ <- satisfy (isIdentifier "Attribute"); return TAttribute)
  <|> (do _ <- satisfy (isIdentifier "Msg"); return TMsg)
  <|> parseListType
  <|> parseHtmlType
  <|> (do matchTok L.TokLParen; t <- parseType; matchTok L.TokRParen; return t)

-- BNF: <list_type> ::= "List" <basic_type>
-- Parses list types (e.g., `List Int`)
parseListType :: Parser Type
parseListType = do
  _ <- satisfy $ isIdentifier "List" -- Expects the keyword "List"
  elementType <- parseBasicType -- The element type can be any basic type (or parenthesized)
  return (TList elementType)

--------------------------------------------------------------------------------
-- 2. Basic Token-to-AST Conversions
--------------------------------------------------------------------------------

-- BNF: <let_start> ::= "let" <identifier> "="
-- Parses the 'let <name> =' part of a function definition
parseLet :: Parser String
parseLet = do
  matchTok L.TokLet
  name <- identifier
  matchTok L.TokEquals
  return name

parseIfElse :: Parser Expr
parseIfElse = do
  matchTok L.TokIf
  cond <- parseExpr
  matchTok L.TokThen
  thenBranch <- parseExpr
  matchTok L.TokElse
  IfThenElse cond thenBranch <$> parseExpr

-- BNF: <func_arrow> ::= "=>"
-- Parses the '=>' arrow
parseArrow :: Parser ()
parseArrow = void (matchTok L.TokArrow)

-- Helpers to check if a token is an identifier
isIdentifier :: String -> L.Token -> Bool
isIdentifier expected tok = case L.tokType tok of
  L.TokIdentifier given -> given == expected
  _ -> False

isIdentifier' :: L.Token -> Bool
isIdentifier' tok = case L.tokType tok of
  L.TokIdentifier _ -> True
  _ -> False

-- BNF: <string_literal> ::= <string_token>
-- Parses a string literal
parseStringLiteral :: Parser Expr
parseStringLiteral = LitString <$> stringLit

-- BNF: <int_literal> ::= <number_token>
-- Parses an integer literal
parseIntLiteral :: Parser Expr
parseIntLiteral = LitInt <$> number

-- BNF: <bool_literal> ::= "True" | "False"
-- Parses a boolean literal
parseBoolLiteral :: Parser Expr
parseBoolLiteral = LitBool <$> boolLit

-- BNF: <variable> ::= <identifier>
-- Parses a variable reference
parseVariable :: Parser Expr
parseVariable = Var <$> identifier

-- BNF: <list_literal> ::= "[" ( <expr> ("," <expr>)* )? "]"
-- Parses a list literal (e.g., [1, 2, 3])
parseListLiteral :: Parser Expr
parseListLiteral = do
  _ <- matchTok L.TokLBracket
  elements <- parseExpr `sepBy` matchTok L.TokComma
  _ <- matchTok L.TokRBracket
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

isOrOp tok = case L.tokType tok of
  L.TokPipePipe -> Just Or
  _ -> Nothing

isAndOp tok = case L.tokType tok of
  L.TokAmpAmp -> Just And
  _ -> Nothing

isComparisonOp tok = case L.tokType tok of
  L.TokEqEq -> Just Eq
  L.TokBangEq -> Just Neq
  L.TokLt -> Just Lt
  L.TokGt -> Just Gt
  L.TokLtEq -> Just Le
  L.TokGtEq -> Just Ge
  _ -> Nothing

isAdditiveOp tok = case L.tokType tok of
  L.TokPlus -> Just Add
  L.TokMinus -> Just Sub
  _ -> Nothing

isMultiplicativeOp tok = case L.tokType tok of
  L.TokStar -> Just Mul
  L.TokSlash -> Just Div
  _ -> Nothing
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
parseConcatExpr = chainl1'Expr parseMultiplicativeExpr (try (do _ <- matchTok L.TokStrConcat; return Concat))

-- BNF: <multiplicative_expr> ::= <unary_expr> ( ( "*" | "/" ) <unary_expr> )*
parseMultiplicativeExpr :: Parser Expr
parseMultiplicativeExpr = chainl1'Expr parseUnaryExpr (parseBinOpToken isMultiplicativeOp)

-- BNF: <unary_expr> ::= "!" <unary_expr> | <term>
parseUnaryExpr :: Parser Expr
parseUnaryExpr =
      (do _ <- matchTok L.TokBang -- Expect '!' for 'not'
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
isBinOpToken tok = case L.tokType tok of
  L.TokPlus -> Just Add
  L.TokMinus -> Just Sub
  L.TokStar -> Just Mul
  L.TokSlash -> Just Div
  L.TokAmpAmp -> Just And
  L.TokPipePipe -> Just Or
  L.TokEqEq -> Just Eq
  L.TokBangEq -> Just Neq
  L.TokLt -> Just Lt
  L.TokGt -> Just Gt
  L.TokLtEq -> Just Le
  L.TokGtEq -> Just Ge
  _ -> Nothing

parseAtom :: Parser Expr
parseAtom =
      parseStringLiteral
  <|> parseBoolLiteral
  <|> parseIntLiteral
  <|> try (do -- <<< ADD THIS BLOCK to parse the Unit literal '()'
              _ <- matchTok L.TokLParen
              _ <- matchTok L.TokRParen
              return LitUnit)
  <|> try parseIfElse
  <|> try parseBlock
  <|> try parseListLiteral
  <|> try (do v <- parseVariable; notFollowedBy (matchTok L.TokColon); return v)
  <|> parseDoBlock
  <|> (do -- This now handles both regular parenthesized expressions AND (op)
          _ <- matchTok L.TokLParen
          expr <- try (do -- Operator as Function: (op)
                          op <- satisfy (isJust . isBinOpToken)
                          return $ OpAsFunction (fromJust $ isBinOpToken op)
                      )
              <|> parseExpr -- Fallback to a regular expression
          _ <- matchTok L.TokRParen
          return expr)

--------------------------------------------------------------------------------
-- 4. Block and DoBlock Parsing
--------------------------------------------------------------------------------

-- BNF: <block> ::= "{" <expr>* "}"
parseBlock :: Parser Expr
parseBlock = do
  matchTok L.TokLBrace
  exprs <- parseExpr `sepEndBy` matchTok L.TokSemicolon
  matchTok L.TokRBrace
  return (Block exprs)

-- BNF: <do_block> ::= "do" "{" <expr>* "}"
parseDoBlock :: Parser Expr
parseDoBlock = do
  matchTok L.TokDo
  matchTok L.TokLBrace
  exprs <- many parseExpr
  matchTok L.TokRBrace
  return (DoBlock exprs)

--------------------------------------------------------------------------------
-- 5. Function & Top-Level Declaration Parsing
--------------------------------------------------------------------------------

parseParamName :: Parser String
parseParamName = identifier

parseTypeDeclaration :: Parser (String, Type)
parseTypeDeclaration = do
  name <- identifier
  matchTok L.TokColon
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
  matchTok L.TokRequires
  parseEffectNames

-- BNF: <effect_names> ::= <effect> ("," <effect>)*
parseEffectNames :: Parser [Effect]
parseEffectNames = parseEffectName `sepBy` matchTok L.TokComma

-- BNF: <effect> ::= "ConsoleWrite" | "FileIO" | "Network"
parseEffectName :: Parser Effect
parseEffectName = do
  eff <- identifier  -- Use our helper
  case eff of
    "ConsoleWrite" -> return ConsoleWrite
    "FileIO"       -> return FileIO
    "Network"      -> return Network
    _              -> fail ("Unknown effect: " ++ eff)

--------------------------------------------------------------------------------
-- 7. Program Parsing (Alternative robust strategy for top-level declarations)
--------------------------------------------------------------------------------

-- A sum type to represent either a type declaration or a function definition found at the top level
data TopLevelDecl = TypeDecl (String, Type) | FuncDef Function deriving Show

-- BNF: <top_level_declaration> ::= <type_declaration> | <function_definition>
-- The main program parser: collects all top-level declarations and merges them
parseTopLevelDeclaration :: Parser TopLevelDecl
parseTopLevelDeclaration =
      TypeDecl <$> try parseTypeDeclaration
  <|> FuncDef <$> parseFunctionDefinition

-- BNF: <program> ::= <top_level_declaration>* <EOF>
parseProgram :: Parser [Function]
parseProgram = do
  decls <- many parseTopLevelDeclaration
  -- traceShow decls (pure ())
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

  matchTok L.TokEOF -- This line remains at the end
  eof -- Make sure there's nothing after the EOF token
  return functionsWithTypes

-- Simple helper to find duplicates in a list (for basic error checking)
findDuplicates :: Ord a => [a] -> [a]
findDuplicates xs = Map.keys $ Map.filter (>1) $ Map.fromListWith (+) [(x, 1) | x <- xs]
