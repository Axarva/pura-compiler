module Permissions where

import AST
import Types (GlobalEnv)

-- Check a single function's declared effects against its used effects
checkFunction :: GlobalEnv -> Function -> Either String ()
checkFunction globalEnv (Function name _ _ body declaredEffects) =
  let usedEffects = gatherEffects body
  in if all (`elem` declaredEffects) usedEffects
        then Right ()
        else Left $ "Function " ++ name ++ " uses undeclared effects: " ++ show usedEffects ++
                    ". Declared: " ++ show declaredEffects ++ ". Used: " ++ show usedEffects

-- Recursively gathers effects from an expression
-- The argument 'globalEnv' is now omitted from recursive calls because it is unused
gatherEffects :: Expr -> [Effect]
gatherEffects expr = case expr of
  Var _         -> []
  LitInt _      -> []
  LitString _   -> []
  LitBool _     -> []
  LitUnit      -> []
  OpAsFunction _ -> []
  LitList elements -> concatMap gatherEffects elements
  Concat e1 e2  -> gatherEffects e1 ++ gatherEffects e2
  BinOp _ e1 e2 -> gatherEffects e1 ++ gatherEffects e2
  UnOp _ e1     -> gatherEffects e1
  Block exprs   -> concatMap gatherEffects exprs
  DoBlock exprs -> concatMap gatherEffects exprs
  IfThenElse cond thenE elseE -> gatherEffects cond ++ gatherEffects thenE ++ gatherEffects elseE
  Let _ val body -> gatherEffects val ++ gatherEffects body
  -- Handle Application (Checking for restricted functions like 'print')
  Apply funcExpr argExpr ->
    let funcEffects = gatherEffects funcExpr
        argEffects  = gatherEffects argExpr
        callEffect  = case funcExpr of
                        Var "print" -> [ConsoleWrite]
                        Var "prompt" -> [BrowserPrompt]
                        -- In the future, we would look up 'funcExpr' in GlobalEnv
                        -- to see if that function requires effects.
                        _           -> [] 
    in callEffect ++ funcEffects ++ argEffects