module Main where

import System.IO (readFile, writeFile, hSetEncoding, stdout, stderr, utf8)
import System.Environment (getArgs)
import Text.Megaparsec (runParser, errorBundlePretty) -- Changed 'parse' to 'runParser' for clarity
import Lexer (tokenize)
import Parser (parseProgram)
import TypeChecker (checkProgram)
import Permissions (checkFunction)

import qualified CodeGen as CG
import System.FilePath ((<.>)) -- for paths

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  args <- getArgs
  case args of
    [filePath] -> do
      sourceCode <- readFile filePath
      putStrLn $ "Compiling: " ++ filePath

      -- 1. Lexing
      let tokens = tokenize sourceCode
      -- print tokens

      -- 2. Parsing
      case runParser parseProgram filePath tokens of
        Left errBundle -> do
          putStrLn "--- Parsing Failed ---"
          putStrLn (errorBundlePretty errBundle)
        Right functions -> do
          -- print functions
          putStrLn "--- Parsing Successful ---"

          -- 3. Type Checking
          case checkProgram functions of
            Left errMsg -> do
              putStrLn "--- Type Checking Failed ---"
              putStrLn errMsg
            Right globalEnv -> do
              putStrLn "--- Type Checking Successful ---"

              -- 4. Effect Checking
              case mapM_ (checkFunction globalEnv) functions of
                Left errMsg -> do
                  putStrLn "--- Effect Checking Failed ---"
                  putStrLn errMsg
                Right () -> do
                  putStrLn "--- Effect Checking Successful ---"

                  -- 5. Code Generation for MVU
                  putStrLn "--- Generating Code ---"
                  let outPath = filePath <.> "js"
                  let generatedCode = CG.generateProgram functions -- Using the function from CodeGen.hs
                  
                  writeFile outPath generatedCode
                  -- putStrLn generatedCode
                  putStrLn "----------------------"
                  putStrLn $ "Successfully wrote generated code to " ++ outPath

    _ -> putStrLn "Usage: pura-compiler <filepath>"