module Main where

import System.Environment (getArgs)
import Text.Megaparsec (parse)
import Lexer (tokenize)
import Parser (parseProgram)
import TypeChecker (checkProgram, GlobalEnv)
import Permissions (checkFunction)
import AST (Function(..))

import qualified CodeGen as CG
import Data.List (partition)

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

                  -- 5. Code Generation
                  putStrLn "Generating code..."

                  -- Separate the main function from the rest
                  let (mainFuncs, otherFuncs) = partition (\f -> funcName f == "main") functions

                  case mainFuncs of
                    -- Handle case where no main function is defined
                    [] -> putStrLn "Compilation warning: No main function found. Nothing to execute."

                    -- This is the success case, where exactly one main function exists
                    [mainFunc] -> do
                      let prelude = "const print = console.log;\nconst toString = (x) => x.toString();\n\n"

                      -- Generate 'const' declarations for all non-main functions
                      let otherFuncCode = unlines $ map CG.generateFunction otherFuncs

                      -- For main, just generate the code for its body expression
                      let mainExprCode = CG.generateExpr (funcBody mainFunc)

                      -- Combine everything into the final JS file
                      let generatedCode = prelude ++ otherFuncCode ++ "\n// --- Execute Main ---\n" ++ mainExprCode ++ ";\n"
                      
                      putStrLn "--- Generated Code ---"
                      putStrLn generatedCode
                      putStrLn "----------------------"

                      let outPath = filePath ++ ".js"
                      writeFile outPath generatedCode
                      putStrLn $ "Successfully wrote generated code to " ++ outPath

                    -- Handle case where multiple main functions are defined
                    _ -> putStrLn "Compilation error: Multiple main functions defined."

    _ -> putStrLn "Usage: pura-compiler <filepath>"
