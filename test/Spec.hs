module Main where

import Test.Hspec

-- Imports
import Lexer (tokenize, TokenType(..), Token(..))
import Parser (parseProgram)
import TypeChecker (checkProgram)
import Permissions (checkFunction)
import AST (Function(..), Expr(..), BinOperator(..))
import Types (Type(..))
import qualified CodeGen as CG

import Text.Megaparsec (runParser, errorBundlePretty)
import Data.Either (isLeft, isRight)
import Control.Monad ((>=>))
import System.FilePath ((<.>), (</>))

-- Helper to ignore token positions for simple tests
stripPos :: [Token] -> [TokenType]
stripPos = map tokType

-- Helper to run compiler pipeline
compileSource :: String -> String -> Either String String
compileSource filename sourceCode = do
  -- 1. Lexing
  let tokens = tokenize sourceCode
  
  -- 2. Parsing
  functions <- case runParser parseProgram filename tokens of
    Left errBundle -> Left (errorBundlePretty errBundle)
    Right funcs    -> Right funcs
    
  -- 3. Type Checking
  globalEnv <- checkProgram functions
  
  -- 4. Effect Checking
  mapM_ (checkFunction globalEnv) functions
    
  -- 5. Code Generation
  return (CG.generateProgram functions)

main :: IO ()
main = hspec $ do
  
  -- Based on: class_materials/verify.md -> Lexer Testing
  describe "Lexer" $ do
    it "tokenizes keywords like 'let'" $ do
      stripPos (tokenize "let") `shouldBe` [TokLet, TokEOF]
      
    it "tokenizes operators like '=>' and '++'" $ do
      stripPos (tokenize "=> ++") `shouldBe` [TokArrow, TokStrConcat, TokEOF]
      
    it "tokenizes literals" $ do
      stripPos (tokenize "123 \"hi\" True") `shouldBe` [TokNumber 123, TokStringLiteral "hi", TokBoolLiteral True, TokEOF]

  -- Based on: class_materials/verify.md -> Parser Testing
  describe "Parser" $ do
    it "parses operator precedence (1 + 2 * 3)" $ do
      let code = "main : Int \n let main = 1 + 2 * 3"
      let tokens = tokenize code
      let (Right [Function { funcBody = body }]) = runParser parseProgram "test" tokens
      -- Expects: 1 + (2 * 3)
      body `shouldBe` (BinOp Add (LitInt 1) (BinOp Mul (LitInt 2) (LitInt 3)))
      
    it "fails on incomplete syntax" $ do
      let code = "main : Int -> \n let main = 1 +"
      let tokens = tokenize code
      runParser parseProgram "test" tokens `shouldSatisfy` isLeft

  -- Based on: class_materials/verify.md -> Type Checker Testing
  describe "TypeChecker" $ do
    it "accepts valid programs (1 + 2)" $ do
      let code = "main : Int \n let main = 1 + 2"
      let (Right funcs) = runParser parseProgram "test" (tokenize code)
      checkProgram funcs `shouldSatisfy` isRight
      
    it "rejects invalid programs (Int + String)" $ do
      let code = "main : Int \n let main = 1 + \"hello\""
      let (Right funcs) = runParser parseProgram "test" (tokenize code)
      checkProgram funcs `shouldSatisfy` isLeft

  -- Acceptance Tests
  describe "Acceptance Tests: Pass" $ do

    -- Test for tree.pura
    it "compiles 'tree.pura' to correct JavaScript" $ do
      let puraFile = "test" </> "golden" </> "pass" </> "tree.pura"
      let goldenFile = puraFile <.> "js" -- "test/golden/pass/tree.pura.js"
      
      puraSource <- readFile puraFile
      goldenJS <- readFile goldenFile
      
      -- Run the compiler pipeline
      let result = compileSource puraFile puraSource
      
      -- Assert that the result is (Right goldenJS)
      result `shouldBe` Right goldenJS

    -- Test for counter.pura
    it "compiles 'counter.pura' to correct JavaScript" $ do
      let puraFile = "test" </> "golden" </> "pass" </> "counter.pura"
      let goldenFile = puraFile <.> "js"
      
      puraSource <- readFile puraFile
      goldenJS <- readFile goldenFile
      
      let result = compileSource puraFile puraSource
      result `shouldBe` Right goldenJS

  describe "Acceptance Tests: Fail" $ do
  
    it "fails to type-check a type mismatch" $ do
      let puraFile = "test" </> "golden" </> "fail" </> "type_mismatch.pura"
      
      puraSource <- readFile puraFile
      let result = compileSource puraFile puraSource
      
      -- Just care that it failed, not how
      result `shouldSatisfy` isLeft

    it "fails to effect-check a missing REQUIRES" $ do
      let puraFile = "test" </> "golden" </> "fail" </> "missing_effect.pura"

      puraSource <- readFile puraFile
      let result = compileSource puraFile puraSource
      
      result `shouldSatisfy` isLeft
