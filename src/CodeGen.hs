-- src/CodeGen.hs
module CodeGen (generateCode) where

import AST
import Data.Text (Text, unpack, pack)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (intercalate)

-- The data type representing an effect that the Pura runtime will execute
data RuntimeEffect = RConsoleWrite Text
                   -- Add more runtime effects as your language grows
                   deriving (Show, Eq)

-- Generates the Haskell code that will be executed by the PuraRuntime.hs
-- It mainly produces the 'mainPuraEffects' list.
generateCode :: Program -> Text
generateCode (Program decls) =
    let
        -- Filter out the MainDecl for specific processing
        mainDecl = case filter isMainDecl decls of
            [m] -> m
            _   -> error "Program must have exactly one main declaration" -- Error handling

        -- Compile the main body into a list of RuntimeEffects
        mainEffectsCode = compileExpr (mainBody mainDecl)
    in
    preamble ++
    (intercalate "\n\n" . map compileDecl) decls ++ "\n\n" ++
    mainExecutionPart (unpack mainEffectsCode)

  where
    isMainDecl (MainDecl _ _) = True
    isMainDecl _ = False

    preamble :: Text
    preamble =
      "module GeneratedPura where\n\n" <>
      "import Runtime\n" <>
      "import Data.Text (Text, pack)\n\n" <>
      -- Define the generated effect data type if not imported
      "data GeneratedPuraEffect = GConsoleWrite Text deriving (Show, Eq)\n\n"

    -- Compiles a single declaration (function or main)
    compileDecl :: Decl -> Text
    compileDecl (FuncDecl name params body _) =
      unpack name <> " :: " <> typeSignatureForBody body <> "\n" <>
      unpack name <> " " <> (intercalate " " (map unpack params)) <> " = " <> compileExpr body
    compileDecl (MainDecl _ _) = "" -- Main is handled separately in mainExecutionPart

    -- Simplified: determines the return type for generated Haskell function
    -- In a full compiler, this would be done by a type checker
    typeSignatureForBody :: Expr -> Text
    typeSignatureForBody expr = case expr of
      EInt _ -> "Int"
      EString _ -> "Text"
      EEffectOp PrintOp _ -> "[GeneratedPuraEffect]" -- Functions returning effects return list of effects
      EBinOp Concat _ _ -> "Text"
      EApp _ _ -> "Text" -- Assuming String for now
      EVar _ -> "Text"
      EBlock _ -> "[GeneratedPuraEffect]" -- Blocks are for effects for now
      _ -> "Text" -- Default for safety


    -- Compiles an expression into Haskell code
    compileExpr :: Expr -> Text
    compileExpr expr = case expr of
      EInt i -> pack $ show i
      EString s -> pack $ show s -- Haskell needs Text string literals quoted
      EVar name -> name
      EBinOp Concat e1 e2 ->
        "(" <> compileExpr e1 <> " <> " <> compileExpr e2 <> ")" -- Use <> for Text concatenation
      EApp funcName args ->
        funcName <> " " <> (intercalate " " (map compileExpr args))
      EEffectOp PrintOp e ->
        "[GConsoleWrite (" <> compileExpr e <> ")]" -- Generate an effect descriptor
      EBlock exprs ->
        -- For simplicity, a block compiles to a list of effects (the last one could be a value)
        -- This will need refinement for full language features
        pack $ "concat (\n" ++ (intercalate "\n" $ map (unpack . compileExpr) exprs) ++ ")"

      _ -> error $ "Unsupported expression in CodeGen: " ++ show expr


    -- The main execution part, which calls the runtime
    mainExecutionPart :: Text -> Text
    mainExecutionPart compiledMainBody =
      "main :: IO ()\n" <>
      "main = executePuraProgram (\n" <>
      compiledMainBody <> "\n)\n"