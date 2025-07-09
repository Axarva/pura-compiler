-- src/EffectChecker.hs
module EffectChecker (checkEffects) where

import AST
import Data.Text (Text)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State (StateT, get, put, evalStateT)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad (foldM)

-- A simplified type for effect checking.
-- In a real compiler, this would be much more sophisticated (e.g., including data types)
data FuncInfo = FuncInfo
  { fiParams :: [Text]
  , fiBody :: Expr
  , fiDeclaredEffects :: Set Effect
  } deriving (Show, Eq)

-- Environment for effect checking: maps function names to their info
type EffectEnv = Map Text FuncInfo

-- The monad stack for effect checking:
--   ExceptT for error handling (Left String on error)
--   StateT for managing the EffectEnv (adding function info as we go)
type CheckM a = ExceptT String (StateT EffectEnv IO) a

-- Main function to check effects of a program
checkEffects :: Program -> IO (Either String Program)
checkEffects (Program decls) = evalStateT (runExceptT (checkProgramM decls)) Map.empty

checkProgramM :: [Decl] -> CheckM [Decl]
checkProgramM decls = do
    -- Pass 1: Collect all function signatures and their declared effects
    -- This populates the EffectEnv
    env <- get
    newEnv <- foldM collectSignatures env decls
    put newEnv

    -- Pass 2: Check each declaration's body for actual effects vs. declared effects
    -- This is where the core verification happens
    mapM_ checkDeclEffects decls
    pure decls -- If all checks pass, return the original declarations

-- Collects declared effects into the environment
collectSignatures :: EffectEnv -> Decl -> CheckM EffectEnv
collectSignatures env (FuncDecl name params body declaredEffects) =
    if Map.member name env
    then throwError $ "Error: Function '" ++ show name ++ "' redefined."
    else pure $ Map.insert name (FuncInfo params body declaredEffects) env
collectSignatures env (MainDecl _ _) = pure env -- main isn't called like a function

-- Checks effects for a single declaration (function or main)
checkDeclEffects :: Decl -> CheckM ()
checkDeclEffects (FuncDecl name _ body declaredEffects) = do
    env <- get
    actualEffects <- collectEffectsFromExpr body env
    let missingEffects = actualEffects `Set.difference` declaredEffects
    let undeclaredEffects = declaredEffects `Set.difference` actualEffects -- For warning about unused declarations

    if Set.null missingEffects
    then pure () -- All good
    else throwError $ "Error: Function '" ++ show name ++
                      "' performs undeclared effects: " ++ show (Set.toList missingEffects)
checkDeclEffects (MainDecl body declaredEffects) = do
    env <- get
    actualEffects <- collectEffectsFromExpr body env
    let missingEffects = actualEffects `Set.difference` declaredEffects
    if Set.null missingEffects
    then pure ()
    else throwError $ "Error: 'main' program performs undeclared effects: " ++ show (Set.toList missingEffects)


-- Recursively collects all actual effects performed by an expression
collectEffectsFromExpr :: Expr -> EffectEnv -> CheckM (Set Effect)
collectEffectsFromExpr expr env = case expr of
    EBlock exprs ->
        foldM (\acc e -> Set.union acc <$> collectEffectsFromExpr e env) Set.empty exprs
    EApp funcName _ ->
        case Map.lookup funcName env of
            Just (FuncInfo _ _ declared) -> pure declared
            Nothing -> throwError $ "Error: Call to undefined function '" ++ show funcName ++ "'"
    EEffectOp PrintOp _ -> pure $ Set.singleton ConsoleWrite -- 'print' has ConsoleWrite effect
    -- Pure expressions have no direct effects themselves
    EInt _ -> pure Set.empty
    EString _ -> pure Set.empty
    EVar _ -> pure Set.empty
    EBinOp _ e1 e2 -> do
        effects1 <- collectEffectsFromExpr e1 env
        effects2 <- collectEffectsFromExpr e2 env
        pure $ Set.union effects1 effects2
    _ -> pure Set.empty -- Add more cases as your language grows