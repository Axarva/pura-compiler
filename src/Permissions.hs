module Permissions where

import AST

checkFunction :: Function -> Either String ()
checkFunction (Function name _ body declaredEffects) =
  let usedEffects = gatherEffects body
  in if all (`elem` declaredEffects) usedEffects
        then Right ()
        else Left $ "Function " ++ name ++ " uses undeclared effects: " ++ show usedEffects

gatherEffects :: Expr -> [Effect]
gatherEffects (Var _) = []
gatherEffects (LitInt _) = []
gatherEffects (LitString _) = []
gatherEffects (Add e1 e2) = gatherEffects e1 ++ gatherEffects e2
gatherEffects (Concat e1 e2) = gatherEffects e1 ++ gatherEffects e2
gatherEffects (Call funcName args) =
  if funcName == "print"
    then [ConsoleWrite]
    else concatMap gatherEffects args
