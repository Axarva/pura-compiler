-- src/Parser.hs
module Parser (pProgram, parsePura) where

import AST
import Token
import Data.Text (Text)
import Data.Set (Set, fromList)
import Text.Megaparsec
import Control.Monad (void)

-- Helper for parsing specific tokens
t :: Token -> Parser Token
t expected = satisfy (== expected) <?> show expected

-- Parser for a generic identifier (from Token.hs)
pIdentifier :: Parser Text
pIdentifier = do
  TIdentifier name <- satisfy isIdentifierToken <?> "identifier"
  pure name
  where isIdentifierToken (TIdentifier _) = True
        isIdentifierToken _ = False

-- Parser for a string literal
pString :: Parser Text
pString = do
  TString s <- satisfy isStringToken <?> "string literal"
  pure s
  where isStringToken (TString _) = True
        isStringToken _ = False

-- Parser for an integer literal
pInt :: Parser Int
pInt = do
  TInt i <- satisfy isIntToken <?> "integer literal"
  pure i
  where isIntToken (TInt _) = True
        isIntToken _ = False

-- Parser for an effect name
pEffectName :: Parser Effect
pEffectName = do
  TEffectName name <- satisfy isEffectNameToken <?> "effect name"
  case name of
    "ConsoleWrite" -> pure ConsoleWrite
    "RandomGen"    -> pure RandomGen
    "FilesystemIO" -> pure FilesystemIO
    _              -> customFailure $ "Unknown effect name: " ++ show name
  where isEffectNameToken (TEffectName _) = True
        isEffectNameToken _ = False

-- Parser for a list of effects in a REQUIRES clause
pRequiresClause :: Parser (Set Effect)
pRequiresClause = do
  void (t TRequires)
  effectNames <- sepBy1 pEffectName (t TComma)
  pure $ fromList effectNames

-- Parser for a single expression
pExpr :: Parser Expr
pExpr = pBinOpConcat

pBinOpConcat :: Parser Expr
pBinOpConcat = do
  expr <- pTerm
  optional (do
    void (t TPlusPlus)
    EBinOp Concat expr <$> pBinOpConcat)
    <|> pure expr

pTerm :: Parser Expr
pTerm = choice
  [ EInt <$> pInt
  , EString <$> pString
  , pAppOrVar
  , EEffectOp PrintOp <$> (void (t TBuiltinPrint) *> t TOpenParen *> pExpr <* t TCloseParen)
  , EEffectOp (EffectOpBuiltin "toString") <$> (void (t TBuiltinToString) *> t TOpenParen *> pExpr <* t TCloseParen) -- toString built-in
  , EBlock <$> (void (t TOpenBrace) *> many pExpr <* void (t TCloseBrace)) -- Block expression
  , (t TOpenParen *> pExpr <* t TCloseParen) -- Parenthesized expression
  ]

-- Helper for distinguishing function application from variable reference
pAppOrVar :: Parser Expr
pAppOrVar = do
  name <- pIdentifier
  optional (t TOpenParen) >>= \case
    Just _ -> EApp name <$> sepBy pExpr (t TComma) <* t TCloseParen
    Nothing -> pure (EVar name)

-- Parser for a function declaration
pFuncDecl :: Parser Decl
pFuncDecl = do
  void (t TLet)
  name <- pIdentifier
  void (t TOpenParen)
  params <- sepBy pIdentifier (t TComma)
  void (t TCloseParen)
  void (t TArrow)
  body <- pExpr
  effects <- optional pRequiresClause
  pure $ FuncDecl name params body (maybe Set.empty id effects)

-- Parser for the main declaration
pMainDecl :: Parser Decl
pMainDecl = do
  void (t (TIdentifier "main")) -- 'main' treated as a special identifier for simplicity
  void (t TEquals)
  body <- EBlock <$> (void (t TOpenBrace) *> many pExpr <* void (t TCloseBrace))
  effects <- optional pRequiresClause
  pure $ MainDecl body (maybe Set.empty id effects)

-- Parser for a top-level declaration (either function or main)
pDecl :: Parser Decl
pDecl = pFuncDecl <|> pMainDecl

-- Parser for the entire program
pProgram :: Parser Program
pProgram = Program <$> many pDecl <* t TEOF

-- Helper function to run the parser
parsePura :: FilePath -> [Token] -> Either (ParseErrorBundle Text String) Program
parsePura filePath = parse pProgram filePath