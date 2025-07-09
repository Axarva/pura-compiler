-- src/Lexer.hs
module Lexer (pTokens, lexPura) where

import Token
import Data.Text (Text, pack)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L

-- Parser for consuming whitespace and comments
sc :: Parser () -- 'space consumer'
sc = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

-- Parsers for individual tokens
pKeyword :: Text -> Token -> Parser Token
pKeyword kw tok = tok <$ lexeme (string kw <* notFollowedBy alphaNumChar)

pIdentifier :: Parser Token
pIdentifier = TIdentifier . pack <$> lexeme ((:) <$> letterChar <*> many alphaNumChar)

pString :: Parser Token
pString = TString . pack <$> lexeme (char '"' *> manyTill L.charLiteral (char '"'))

pInt :: Parser Token
pInt = TInt <$> lexeme L.decimal

pEffectName :: Parser Token
pEffectName = TEffectName . pack <$> lexeme (many (upperChar <|> char '_'))

-- Combined parser for all tokens
pToken :: Parser Token
pToken = choice
  [ pKeyword "let" TLet
  , pKeyword "REQUIRES" TRequires
  , pKeyword "print" TBuiltinPrint
  , pKeyword "toString" TBuiltinToString
  , symbol "=" *> pure TEquals
  , symbol "=>" *> pure TArrow
  , symbol "{" *> pure TOpenBrace
  , symbol "}" *> pure TCloseBrace
  , symbol "(" *> pure TOpenParen
  , symbol ")" *> pure TCloseParen
  , symbol "," *> pure TComma
  , symbol "++" *> pure TPlusPlus
  , symbol "-" *> pure 
  , pEffectName -- Must come before pIdentifier if effect names can look like identifiers
  , pIdentifier
  , pString
  , pInt
  ]

-- Parser for the entire list of tokens
pTokens :: Parser [Token]
pTokens = many pToken <* eof

-- Helper function to run the lexer
lexPura :: FilePath -> Text -> Either (ParseErrorBundle Text String) [Token]
lexPura filePath = parse pTokens filePath