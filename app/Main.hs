-- app/Main.hs
module Main (main) where

import System.Environment (getArgs)
import Data.Text (Text, pack, unpack)
import Lexer (lexPura)
import Parser (parsePura)
import EffectChecker (checkEffects)
import CodeGen (generateCode)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import System.Process (rawSystem) -- For running generated Haskell code

-- The main function of your Pura compiler
main :: IO ()
main = do
    args <- getArgs
    case args of
        [filePath] -> do
            sourceCode <- readFileText filePath
            processFile filePath sourceCode
        _ -> hPutStrLn stderr "Usage: Pura-compiler-exe <filepath.Pura>"

processFile :: FilePath -> Text -> IO ()
processFile filePath sourceCode = do
    putStrLn $ "--- Lexing " ++ filePath ++ " ---"
    lexResult <- lexPura filePath sourceCode
    case lexResult of
        Left err -> do
            hPutStrLn stderr $ "Lexing Error:\n" ++ errorBundlePretty err
            exitFailure
        Right tokens -> do
            putStrLn $ "Tokens: " ++ show tokens
            putStrLn $ "\n--- Parsing " ++ filePath ++ " ---"
            parseResult <- parsePura filePath tokens
            case parseResult of
                Left err -> do
                    hPutStrLn stderr $ "Parsing Error:\n" ++ errorBundlePretty err
                    exitFailure
                Right ast -> do
                    putStrLn $ "AST:\n" ++ show ast
                    putStrLn $ "\n--- Effect Checking " ++ filePath ++ " ---"
                    checkResult <- checkEffects ast
                    case checkResult of
                        Left err -> do
                            hPutStrLn stderr $ "Effect Checking Error:\n" ++ err
                            exitFailure
                        Right verifiedAst -> do
                            putStrLn "Effect checking successful!"
                            putStrLn $ "\n--- Generating Code for " ++ filePath ++ " ---"
                            let generatedHaskellCode = generateCode verifiedAst
                            let outputFilePath = "GeneratedPura.hs"
                            writeFileText outputFilePath generatedHaskellCode
                            putStrLn $ "Generated Haskell code written to " ++ outputFilePath
                            putStrLn $ "\n--- Compiling Generated Code ---"
                            -- Use stack to compile the generated Haskell code
                            -- We'll compile it into an executable that calls the runtime
                            let compileCmd = "stack ghc -- -main-is GeneratedPura.main main.hs -o Pura-program-exe GeneratedPura.hs -hide-package Pura-compiler" -- Simplified: needs to link PuraRuntime
                            let compileCmdStack = "stack ghc -- " ++ outputFilePath ++ " src/PuraRuntime.hs -main-is GeneratedPura.mainPuraEffects -o Pura-program-exe -package Pura-compiler"
                            -- Correction: `GeneratedPura.hs` will have `mainPuraEffects :: [PuraEffect]`
                            -- We need to execute that list using `PuraRuntime.executePuraProgram`.
                            -- So, the `Main.hs` needs to be provided with the generated module.

                            -- A simpler way for a demo: Just compile GeneratedPura.hs and run it directly
                            -- It will have its own main defined.
                            let ghcCommand = "stack ghc -- " ++ outputFilePath ++ " src/PuraRuntime.hs"
                            putStrLn $ "Running: " ++ ghcCommand
                            ghcExitCode <- rawSystem "stack" ["ghc", "--", outputFilePath, "src/PuraRuntime.hs"]
                            case ghcExitCode of
                                ExitFailure code -> do
                                    hPutStrLn stderr $ "Error compiling generated Haskell code. Exit code: " ++ show code
                                    exitFailure
                                _ -> do
                                    putStrLn "Generated Haskell code compiled successfully!"
                                    putStrLn "\n--- Running Pura Program ---"
                                    -- Execute the compiled program
                                    PuraExitCode <- rawSystem "./GeneratedPura" []
                                    case PuraExitCode of
                                        ExitFailure code -> hPutStrLn stderr $ "Pura program exited with error code: " ++ show code
                                        _ -> putStrLn "Pura program executed successfully!"

-- Helper for reading/writing Text files
readFileText :: FilePath -> IO Text
readFileText = fmap pack . readFile

writeFileText :: FilePath -> Text -> IO ()
writeFileText fp = writeFile fp . unpack