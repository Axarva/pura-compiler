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
  LitList elements -> concatMap (gatherEffects globalEnv) elements -- Recursively gather from list elements
  Concat e1 e2  -> gatherEffects globalEnv e1 ++ gatherEffects globalEnv e2
  BinOp _ e1 e2 -> gatherEffects globalEnv e1 ++ gatherEffects globalEnv e2
  UnOp _ e1     -> gatherEffects globalEnv e1
  Block exprs   -> concatMap (gatherEffects globalEnv) exprs
  DoBlock exprs -> concatMap (gatherEffects globalEnv) exprs
  Call name args ->
    let argEffects = concatMap (gatherEffects globalEnv) args -- Effects from arguments themselves
    in case Map.lookup name globalEnv of
         -- If it's a known function (e.g., "print"), check its specific effects.
         -- For user-defined functions, you might eventually store declared effects
         -- in GlobalEnv along with types, or deduce from type/body.
         -- For now, we'll hardcode "print" and assume others are pure or have unknown effects.
         Just _ -> -- Function found in globalEnv (its type is known)
           if name == "print"
             then ConsoleWrite : argEffects
             -- For other user-defined functions, you might need their declared effects.
             -- For simplicity now, assume only built-ins have implicit effects or
             -- effects are deduced during checkProgram.
             else argEffects -- Default to no additional effects for other calls for now
         Nothing -> -- Function not found (could be a type error, but effect checker tries best)
           argEffects -- Cannot determine effects of unknown function
