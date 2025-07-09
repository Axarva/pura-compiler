module Main where

import Parser
import Lexer
import Text.Megaparsec (parse)
import Data.Void (Void)


main :: IO ()
main = do
  source <- readFile "example.pura"
  let tokens = tokenize source
  print tokens
  let result = parse parseFunction "<test>" tokens
  case result of
    Left err -> putStrLn ("Parse error: " ++ show err)
    Right fn -> putStrLn ("Parsed function: " ++ show fn)