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
import Debug.Trace -- Keep for debugging, remove when done

-- Parser monad type
type Parser = Parsec Void [L.Token]

-- Megaparsec Stream and VisualStream instances for custom tokens
-- These are necessary for errorBundlePretty to work properly
-- instance Stream [L.Token] where
--   type Token [L.Token] = L.Token
--   type Tokens [L.Token] = [L.Token]

--   take1_ (t:ts) = Just (t, ts)
--   take1_ [] = Nothing

--   takeN_ n s
--     | n <= 0 = Just ([], s)
--     | null s = Nothing
--     | otherwise = Just (splitAt n s)

--   tokensLength _ = length
--   showTokens _ = show

-- instance VisualStream [L.Token] where
--   tokenWidth _ = length . show
--   tokensToString _ = show


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

-- Main entry for parsing a type
parseType :: Parser Type
parseType = parseArrowType

-- Parses function arrow types (right-associative: A -> B -> C is A -> (B -> C))
parseArrowType :: Parser Type
parseArrowType = chainr1'Type parseBasicType (do _ <- single L.TokRightArrow; return TArr)

-- Parses atomic or parenthesized types
parseBasicType :: Parser Type
parseBasicType =
      (do L.TokIdentifier "Int"    <- satisfy (isIdentifier "Int"); return TInt)
  <|> (do L.TokIdentifier "String" <- satisfy (isIdentifier "String"); return TString)
  <|> (do L.TokIdentifier "Bool"   <- satisfy (isIdentifier "Bool"); return TBool)
  <|> (do L.TokIdentifier "Unit"   <- satisfy (isIdentifier "Unit"); return TUnit)
  <|> parseListType                -- Handles `List T` syntax
  <|> (do _ <- single L.TokLParen; t <- parseType; _ <- single L.TokRParen; return t) -- For `(A -> B)`

-- Parses list types (e.g., `List Int`)
parseListType :: Parser Type
parseListType = do
  L.TokIdentifier "List" <- satisfy $ isIdentifier "List" -- Expects the keyword "List"
  elementType <- parseBasicType -- The element type can be any basic type (or parenthesized)
  return (TList elementType)

--------------------------------------------------------------------------------
-- 2. Basic Token-to-AST Conversions
--------------------------------------------------------------------------------

-- Parses the 'let <name> =' part of a function definition
parseLet :: Parser String
parseLet = do
  _ <- single L.TokLet
  L.TokIdentifier name <- satisfy $ isIdentifier'
  _ <- single L.TokEquals
  return name

-- Parses the '=>' arrow
parseArrow :: Parser ()
parseArrow = void (single L.TokArrow)

-- Helpers to check if a token is an identifier

isIdentifier expected (L.TokIdentifier given) = given == expected 
isIdentifier _ _ = False

isIdentifier' :: L.Token -> Bool
isIdentifier' (L.TokIdentifier id) = True
isIdentifier' _ = False

-- Parses a string literal
parseStringLiteral :: Parser Expr
parseStringLiteral = do
  L.TokStringLiteral s <- satisfy (\case L.TokStringLiteral _ -> True; _ -> False)
  return (LitString s)

-- Parses an integer literal
parseIntLiteral :: Parser Expr
parseIntLiteral = do
  L.TokNumber n <- satisfy (\case L.TokNumber _ -> True; _ -> False)
  return (LitInt n)

-- Parses a boolean literal
parseBoolLiteral :: Parser Expr
parseBoolLiteral = do
  L.TokBoolLiteral bool <- satisfy (\case L.TokBoolLiteral _ -> True; _ -> False)
  return (LitBool bool)

-- Parses a variable reference
parseVariable :: Parser Expr
parseVariable = do
  L.TokIdentifier name <- satisfy $ isIdentifier'
  return (Var name)

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
isOrOp :: L.Token -> Maybe BinOperator
isOrOp L.TokPipePipe = Just Or
isOrOp _             = Nothing

isAndOp :: L.Token -> Maybe BinOperator
isAndOp L.TokAmpAmp = Just And
isAndOp _           = Nothing

isComparisonOp :: L.Token -> Maybe BinOperator
isComparisonOp L.TokEqEq   = Just Eq
isComparisonOp L.TokBangEq = Just Neq
isComparisonOp L.TokLt     = Just Lt
isComparisonOp L.TokGt     = Just Gt
isComparisonOp L.TokLtEq   = Just Le
isComparisonOp L.TokGtEq   = Just Ge
isComparisonOp _           = Nothing

isAdditiveOp :: L.Token -> Maybe BinOperator
isAdditiveOp L.TokPlus  = Just Add
isAdditiveOp L.TokMinus = Just Sub
isAdditiveOp _          = Nothing

isMultiplicativeOp :: L.Token -> Maybe BinOperator
isMultiplicativeOp L.TokStar  = Just Mul
isMultiplicativeOp L.TokSlash = Just Div
isMultiplicativeOp _          = Nothing

-- Precedence levels, from lowest to highest:
parseOrExpr :: Parser Expr
parseOrExpr = chainl1'Expr parseAndExpr (parseBinOpToken isOrOp)

parseAndExpr :: Parser Expr
parseAndExpr = chainl1'Expr parseComparisonExpr (parseBinOpToken isAndOp)

parseComparisonExpr :: Parser Expr
parseComparisonExpr = chainl1'Expr parseAdditiveExpr (parseBinOpToken isComparisonOp)

parseAdditiveExpr :: Parser Expr
parseAdditiveExpr = chainl1'Expr parseMultiplicativeExpr (parseBinOpToken isAdditiveOp)

parseMultiplicativeExpr :: Parser Expr
parseMultiplicativeExpr = chainl1'Expr parseUnaryExpr (parseBinOpToken isMultiplicativeOp)

-- String Concatenation has higher precedence than arithmetic, but lower than unary.
parseConcatExpr :: Parser Expr
parseConcatExpr = chainl1'Expr parseUnaryExpr (try (do _ <- single L.TokStrConcat; return Concat))

-- Unary Operators (like 'not' using '!')
parseUnaryExpr :: Parser Expr
parseUnaryExpr =
      (do _ <- single L.TokBang -- Expect '!' for 'not'
          expr <- parseUnaryExpr -- Recursively parse the operand
          return (UnOp Not expr))
  <|> parseConcatExpr -- Fall through to concatenation after unary

-- Term level expressions: atomic values, function calls, parenthesized expressions
parseTerm :: Parser Expr
parseTerm =
      parseCall             -- e.g., greet("World")
  <|> parseStringLiteral
  <|> parseBoolLiteral
  <|> parseIntLiteral
  <|> parseVariable
  <|> parseDoBlock
  <|> parseListLiteral  -- Add list literal parsing
  <|> (do _ <- single L.TokLParen
          expr <- parseExpr -- Allow full expressions inside parentheses
          _ <- single L.TokRParen
          return expr)


--------------------------------------------------------------------------------
-- 4. Block and DoBlock Parsing
--------------------------------------------------------------------------------
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

--------------------------------------------------------------------------------
-- 5. Function & Top-Level Declaration Parsing
--------------------------------------------------------------------------------

-- Parses a function's parameter list (e.g., '(name, age)')
parseParams :: Parser [String]
parseParams = do
  _ <- single L.TokLParen
  params <- parseParamNames `sepBy` single L.TokComma
  _ <- single L.TokRParen
  return params

-- Parses individual parameter names
parseParamNames :: Parser String
parseParamNames = do
  L.TokIdentifier name <- satisfy $ isIdentifier'
  return name

-- Parses a function call (e.g., `func(arg1, arg2)`)
parseCall :: Parser Expr
parseCall = try $ do -- 'try' allows backtracking if it looks like a call but isn't (e.g., just an identifier)
  L.TokIdentifier fname <- satisfy $ isIdentifier'
  _ <- single L.TokLParen  -- Must be '(' immediately after identifier for a call
  args <- parseExpr `sepBy` single L.TokComma
  _ <- single L.TokRParen
  return (Call fname args)

-- Parses a standalone type declaration line (e.g., `myFunction : Int -> String`)
parseTypeDeclaration :: Parser (String, Type)
parseTypeDeclaration = do
  L.TokIdentifier name <- satisfy $ isIdentifier' -- Function name
  _ <- single L.TokColon
  funcSig <- parseType
  return (name, funcSig)

-- Parses a function definition (the `let name = (args) => { body } REQUIRES ...` part)
-- Note: This parser *does not* include the type signature.
parseFunctionDefinition :: Parser Function
parseFunctionDefinition = do
  name <- parseLet
  params <- parseParams
  parseArrow
  body <- parseBlock
  effects <- optional parseRequires
  let effectList = fromMaybe [] effects
  -- Placeholder type signature; will be filled by parseProgram
  return (Function name (TError "Type not yet assigned by top-level parser") params body effectList)

--------------------------------------------------------------------------------
-- 6. Effect Parsing
--------------------------------------------------------------------------------
parseRequires :: Parser [Effect]
parseRequires = do
  _ <- single L.TokRequires
  parseEffectNames

parseEffectNames :: Parser [Effect]
parseEffectNames = do
  parseEffectName `sepBy` single L.TokComma

parseEffectName :: Parser Effect
parseEffectName = do
  L.TokIdentifier eff <- satisfy $ isIdentifier'
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

-- The main program parser: collects all top-level declarations and merges them
-- parseProgram :: Parser [Function]
-- parseProgram = do
--   -- Use a recursive helper to collect all declarations
--   decls <- traceShow "Decls: " parseAllTopLevelDeclarations []

--   -- Separate type declarations and function definitions
--   let typeDeclarations = [ td | TypeDecl td <- decls ]
--   let functionDefinitions = [ fd | FuncDef fd <- decls ]

--   -- Convert type declarations into a Map for easy lookup
--   let typeMap = Map.fromList typeDeclarations

--   -- Assign declared type signatures to their corresponding function definitions
--   let functionsWithTypes = map (\f@Function{funcName} ->
--                                   case Map.lookup funcName typeMap of
--                                     Just t -> f { funcTypeSignature = t }
--                                     Nothing -> error $ "Missing type declaration for function: " ++ funcName ++ ". All top-level functions must have an explicit type declaration."
--                                ) functionDefinitions

--   -- Optional: More robust error checking (e.g., declared type but no definition, duplicate definitions)
--   let declaredButUndefined = Map.keys $ Map.difference typeMap (Map.fromList $ map (\f -> (funcName f, ())) functionsWithTypes)
--   unless (null declaredButUndefined) $
--     fail $ "Error: Type declarations found for undefined functions: " ++ show declaredButUndefined

--   let duplicateDefs = findDuplicates (map funcName functionDefinitions)
--   unless (null duplicateDefs) $
--     fail $ "Error: Duplicate function definitions for: " ++ show duplicateDefs

--   -- Finally, ensure that the entire input file has been consumed and we're at EOF.
--   -- This is crucial to catch unparsed trailing tokens.
--   _ <- single L.TokEOF

--   return functionsWithTypes

-- -- Recursive helper to parse all top-level declarations

parseTopLevelDeclaration :: Parser TopLevelDecl
parseTopLevelDeclaration =
      (TypeDecl <$> try parseTypeDeclaration)
  <|> (FuncDef <$> parseFunctionDefinition)
parseProgram :: Parser [Function]
parseProgram = do
  -- Parse zero or more top-level declarations
  -- 'sepEndBy' is robust for parsing items that can have separators (none here, so 'pure ()')
  -- and also handling trailing separators if they were allowed.
  -- Here, it simply parses individual items until it cannot.
  decls <- parseTopLevelDeclaration `sepEndBy` (pure ()) -- Parse TopLevelDecl until it fails

  -- Perform the merging logic (this part remains the same)
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
    fail $ "Error: Duplicate function definitions for: " ++ show duplicateDefs

  -- Finally, consume the End-Of-File token.
  -- This should be the very last thing that happens after all program content.
  eof -- Megaparsec's built-in EOF parser

  return functionsWithTypes


-- Simple helper to find duplicates in a list (for basic error checking)
findDuplicates :: Ord a => [a] -> [a]
findDuplicates xs = Map.keys $ Map.filter (>1) $ Map.fromListWith (+) [(x, 1) | x <- xs]
