-- In Main.hs
module Main where

import System.Environment (getArgs)
import Text.Megaparsec (parse)
import Lexer (tokenize)
import Parser (parseProgram)
import TypeChecker (checkProgram, GlobalEnv)
import Permissions (checkFunction)
import AST (Function)

main :: IO () -- Corrected type signature
main = do
  args <- getArgs
  case args of
    [filePath] -> do
      sourceCode <- readFile filePath
      putStrLn $ "Compiling: " ++ filePath

      -- 1. Lexing
      let tokens = tokenize sourceCode
      putStrLn "--- Tokens ---"
      print tokens
      putStrLn "--------------"

      -- 2. Parsing
      case parse parseProgram filePath tokens of
        Left err -> do
          putStrLn "Parsing failed!"
          -- Print the raw Megaparsec error bundle. It will be verbose but will compile.
          print err
        Right functions -> do
          putStrLn "Parsing successful!"
          putStrLn "--- AST ---"
          print functions
          putStrLn "-----------"

          -- 3. Type Checking
          case checkProgram functions of
            Left errMsg -> do
              putStrLn "Type checking failed!"
              putStrLn errMsg
            Right globalEnv -> do
              putStrLn "Type checking successful!"
              -- putStrLn "--- Global Type Environment ---"
              -- print globalEnv
              -- putStrLn "-----------------------------"

              -- 4. Effect Checking
              case mapM (checkFunction globalEnv) functions of
                Left errMsg -> do
                  putStrLn "Effect checking failed!"
                  putStrLn errMsg
                Right _ -> do
                  putStrLn "Effect checking successful!"

                  -- 5. Code Generation (Uncomment when ready)
                  -- putStrLn "Generating code..."
                  -- let generatedCode = generateCode functions
                  -- putStrLn "--- Generated Code ---"
                  -- putStrLn generatedCode
                  -- putStrLn "----------------------"
                  -- writeFile (filePath ++ ".js") generatedCode

    _ -> putStrLn "Usage: pura-compiler <filepath>"
