{-# LANGUAGE NamedFieldPuns #-}

module TypeChecker where

import AST
import Types
import Inference 
import qualified Data.Map as Map
import Control.Monad

-- | Helper to infer a single function definition (let-binding)
inferFunction :: GlobalEnv -> Function -> Either String (Subst, Scheme)
inferFunction globalEnv Function{funcName, funcTypeSignature, funcArgs, funcBody} = do
    -- 1. Setup local environment (Monotype variables for arguments)
    (expectedArgTypes, expectedReturnType) <- unpackFunctionType funcTypeSignature (length funcArgs) funcName

    -- Create local Monotype Environment (TVar for each argument)
    let env = Map.fromList (zip funcArgs expectedArgTypes)
    
    -- 2. Infer the function body
    (s_body, t_body) <- runInfer $ inferExpr globalEnv env funcBody
    
    -- 3. The type of the function value itself (t_func) is the fully curried type
    let t_func = foldr TArr (apply s_body t_body) expectedArgTypes

    -- 4. Unify the inferred function type with the declared function type
    s_final <- unify t_func funcTypeSignature
    
    -- 5. Generalize the resulting Monotype into a Scheme (Rule 4)
    let final_type = apply s_final funcTypeSignature
    let scheme = generalize Map.empty final_type
    
    -- Final Check (ensuring the declared return type matches the body's actual type)
    if apply s_final expectedReturnType == apply s_final t_body
      then return (s_final, scheme)
      else Left $ "Type error in function '" ++ funcName ++ "': Declared return type is " ++ show (apply s_final expectedReturnType) ++ " but inferred body type is " ++ show (apply s_final t_body)


-- Helper to unpack a curried function type
unpackFunctionType :: Type -> Int -> String -> Either String ([Type], Type)
unpackFunctionType funcType numArgs funcName = go funcType numArgs []
  where
    go currentType 0 accParams = Right (reverse accParams, currentType)
    go (TArr argType restType) n accParams =
      go restType (n - 1) (argType : accParams)
    go nonFuncType n _ = Left $ "Type error in '" ++ funcName ++ "': Type signature does not have enough arguments. Expected " ++ show numArgs ++ " but signature only provided " ++ show (numArgs - n) ++ ". Found non-function type: " ++ show nonFuncType

-- The main entry point for type checking the entire program
checkProgram :: [Function] -> Either String GlobalEnv
checkProgram funcs = do
  -- Built-in functions must be Schematized (Rule 1)
  let elemType = TArr (TList TAttribute) (TArr (TList (THtml TMsg)) (THtml TMsg))

  let htmlBuiltInSchemes = Map.fromList
        [ ("div", generalize Map.empty elemType)
        , ("p", generalize Map.empty elemType)
        , ("button", generalize Map.empty elemType)
        , ("h1", generalize Map.empty elemType)
        , ("text", generalize Map.empty (TArr TString (THtml TMsg)))
        , ("onClick", generalize Map.empty (TArr TString TAttribute))
        ]

  let stdBuiltInFuncs = Map.fromList
        [ ("toString", generalize Map.empty (TArr TInt TString))
        , ("print", generalize Map.empty (TArr (TVar "a") TUnit))
        , ("id", generalize Map.empty (TArr (TVar "b") (TVar "b")))
        ]
        
  let initialGlobalEnv = Map.union htmlBuiltInSchemes stdBuiltInFuncs
  
  -- 1st pass: gather user function signatures (as Schemes)
  let userFuncSchemes = Map.fromList $ map (\f -> (funcName f, generalize Map.empty (funcTypeSignature f))) funcs

  -- Combine built-ins and user-defined signatures
  let fullGlobalEnv = Map.union userFuncSchemes initialGlobalEnv

  -- 2nd pass: infer and unify all function definitions
  finalSchemes <- forM funcs $ \f -> do
    (s, scheme) <- inferFunction fullGlobalEnv f
    return (funcName f, scheme)

  let finalGlobalEnv = Map.fromList finalSchemes
  
  return (Map.union finalGlobalEnv initialGlobalEnv)
