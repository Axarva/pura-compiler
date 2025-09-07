module Permissions where

import AST
import Types -- Import Type definitions
import qualified Data.Map as Map -- Needed for GlobalEnv

-- Type environment for function names and their types (re-declared for clarity, but it's from Types.hs)
type GlobalEnv = Map.Map String Type

-- Check a single function's declared effects against its used effects
checkFunction :: GlobalEnv -> Function -> Either String ()
checkFunction globalEnv (Function name _ _ body declaredEffects) =
  let usedEffects = gatherEffects globalEnv body -- Pass globalEnv to gatherEffects
  in if all (`elem` declaredEffects) usedEffects
        then Right ()
        else Left $ "Function " ++ name ++ " uses undeclared effects: " ++ show usedEffects ++
                    ". Declared: " ++ show declaredEffects ++ ". Used: " ++ show usedEffects

-- Recursively gathers effects from an expression
gatherEffects :: GlobalEnv -> Expr -> [Effect]
gatherEffects globalEnv expr = case expr of
  Var _         -> []
  LitInt _      -> []
  LitString _   -> []
  LitBool _     -> []
  LitUnit      -> []
  OpAsFunction _ -> []
  LitList elements -> concatMap (gatherEffects globalEnv) elements
  Concat e1 e2  -> gatherEffects globalEnv e1 ++ gatherEffects globalEnv e2
  BinOp _ e1 e2 -> gatherEffects globalEnv e1 ++ gatherEffects globalEnv e2
  UnOp _ e1     -> gatherEffects globalEnv e1
  Block exprs   -> concatMap (gatherEffects globalEnv) exprs
  DoBlock exprs -> concatMap (gatherEffects globalEnv) exprs
  IfThenElse cond thenE elseE -> gatherEffects globalEnv cond ++ gatherEffects globalEnv thenE ++ gatherEffects globalEnv elseE
  -- MODIFIED: Handle the Apply node
  Apply funcExpr argExpr ->
    let funcEffects = gatherEffects globalEnv funcExpr
        argEffects  = gatherEffects globalEnv argExpr
        callEffect  = case funcExpr of
                        -- If we are directly calling a named function, check its effects
                        Var "print" -> [ConsoleWrite]
                        -- In a more advanced system, you'd look up the function's
                        -- effect signature in the GlobalEnv. For now, we only
                        -- hardcode built-ins.
                        _           -> []
    in callEffect ++ funcEffects ++ argEffects