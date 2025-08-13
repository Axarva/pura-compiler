{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module TypeChecker where

import AST
import Types -- Import your Type definitions
import qualified Data.Map as Map
import Control.Monad (foldM, mapM_)
-- import Control.Arrow -- Removed, as &&& is no longer used here

-- Type environment for local variables (parameters, let-bound vars)
type TypeEnv = Map.Map String Type

-- Global environment for function names and their types
type GlobalEnv = Map.Map String Type

--------------------------------------------------------------------------------
-- 1. Helper for Function Call Argument Application
--------------------------------------------------------------------------------

-- Applies an argument's type to a function's type.
-- Takes the global environment (for nested function calls within args),
-- local environment (for variables in args), current function type, and argument expression.
applyArgumentType :: GlobalEnv -> TypeEnv -> Type -> Expr -> Either String Type
applyArgumentType globalEnv localEnv currentFuncType argExpr = do
  argType <- inferExprType globalEnv localEnv argExpr -- Infer type of the argument expression
  case currentFuncType of
    TArr expectedArgType restOfFuncType ->
      if argType == expectedArgType
        then Right restOfFuncType
        else Left $ "Type error: Function expected argument of type " ++ show expectedArgType ++ " but got " ++ show argType ++ " for argument " ++ show argExpr
    _ -> Left $ "Type error: Attempting to apply argument to non-function type " ++ show currentFuncType ++ " when applying " ++ show argExpr

--------------------------------------------------------------------------------
-- 2. Core Expression Type Inference
--------------------------------------------------------------------------------

-- The main type inference function for expressions
inferExprType :: GlobalEnv -> TypeEnv -> Expr -> Either String Type
inferExprType globalEnv localEnv expr = case expr of
  LitInt _    -> Right TInt
  LitString _ -> Right TString
  LitBool _   -> Right TBool
  Var name    -> case Map.lookup name localEnv of
                   Just t  -> Right t
                   Nothing -> case Map.lookup name globalEnv of -- Fallback for global functions
                                Just t -> Right t
                                Nothing -> Left $ "Type error: Undeclared variable '" ++ name ++ "'"

  Concat e1 e2 -> do
    t1 <- inferExprType globalEnv localEnv e1
    t2 <- inferExprType globalEnv localEnv e2
    if t1 == TString && t2 == TString
      then Right TString
      else Left $ "Type error: Cannot concatenate " ++ show t1 ++ " and " ++ show t2 ++ ". Expected String."

  BinOp op e1 e2 -> do
    t1 <- inferExprType globalEnv localEnv e1
    t2 <- inferExprType globalEnv localEnv e2
    case op of
      Add -> checkNumericBinOp TInt "addition" t1 t2
      Sub -> checkNumericBinOp TInt "subtraction" t1 t2
      Mul -> checkNumericBinOp TInt "multiplication" t1 t2
      Div -> checkNumericBinOp TInt "division" t1 t2
      And -> checkBooleanBinOp "AND" t1 t2
      Or  -> checkBooleanBinOp "OR" t1 t2
      Eq  -> if t1 == t2 then Right TBool else Left $ "Type error: Operator '==' expects operands of the same type, got " ++ show t1 ++ " and " ++ show t2
      Neq -> if t1 == t2 then Right TBool else Left $ "Type error: Operator '!=' expects operands of the same type, got " ++ show t1 ++ " and " ++ show t2
      Lt  -> if t1 == TInt && t2 == TInt then Right TBool else Left $ "Type error: Operator '<' expects Int operands, got " ++ show t1 ++ " and " ++ show t2
      Gt  -> if t1 == TInt && t2 == TInt then Right TBool else Left $ "Type error: Operator '>' expects Int operands, got " ++ show t1 ++ " and " ++ show t2
      Le  -> if t1 == TInt && t2 == TInt then Right TBool else Left $ "Type error: Operator '<=' expects Int operands, got " ++ show t1 ++ " and " ++ show t2
      Ge  -> if t1 == TInt && t2 == TInt then Right TBool else Left $ "Type error: Operator '>=' expects Int operands, got " ++ show t1 ++ " and " ++ show t2

  UnOp op e1 -> do
    t1 <- inferExprType globalEnv localEnv e1
    case op of
      Not -> if t1 == TBool
               then Right TBool
               else Left $ "Type error: Operator 'not' expects Bool operand, got " ++ show t1

  Apply funcExpr argExpr -> do
    funcType <- inferExprType globalEnv localEnv funcExpr
    argType <- inferExprType globalEnv localEnv argExpr
    case funcType of
      TArr expectedArgType returnType ->
        if argType == expectedArgType
        then Right returnType
        else Left $ "Type mismatch: function expects argument of type " ++ show expectedArgType ++ " but was given " ++ show argType
      _ -> Left $ "Type error: Cannot apply argument to non-function type " ++ show funcType

  Block exprs -> case reverse exprs of
    []        -> Right TUnit
    (lastE:_) -> inferExprType globalEnv localEnv lastE

  DoBlock exprs -> do
    -- Ensure all expressions inside are type-checked
    mapM_ (inferExprType globalEnv localEnv) exprs
    Right TUnit

  -- List Literals
  LitList elements ->
    if null elements
      then Right (TList TUnit) -- Or a more generic empty list type (e.g., TList TVar if you had polymorphism)
      else do
        firstType <- inferExprType globalEnv localEnv (head elements)
        restTypes <- mapM (inferExprType globalEnv localEnv) (tail elements)
        if all (== firstType) restTypes
          then Right (TList firstType)
          else Left $ "Type error: All elements in a list must have the same type. Expected " ++ show firstType ++ " but got mixed types."

--------------------------------------------------------------------------------
-- 3. Helper Functions for Binary Operator Type Checks
--------------------------------------------------------------------------------

checkNumericBinOp :: Type -> String -> Type -> Type -> Either String Type
checkNumericBinOp expectedType opName t1 t2 =
  if t1 == expectedType && t2 == expectedType
    then Right expectedType
    else Left $ "Type error: Operator '" ++ opName ++ "' expects " ++ show expectedType ++ " operands, got " ++ show t1 ++ " and " ++ show t2

checkBooleanBinOp :: String -> Type -> Type -> Either String Type
checkBooleanBinOp opName t1 t2 =
  if t1 == TBool && t2 == TBool
    then Right TBool
    else Left $ "Type error: Operator '" ++ opName ++ "' expects Bool operands, got " ++ show t1 ++ " and " ++ show t2

--------------------------------------------------------------------------------
-- 4. Function Definition Type Checker
--------------------------------------------------------------------------------

-- Type-checks a single function definition
-- It verifies the function body against its declared type signature.
checkFunctionDefinition :: GlobalEnv -> Function -> Either String ()
checkFunctionDefinition globalEnv Function{funcName, funcTypeSignature, funcArgs, funcBody} = do
  -- 1. Unpack the declared function signature into expected parameter types and the final return type.
  (expectedParamTypes, expectedReturnType) <- unpackFunctionType funcTypeSignature (length funcArgs) funcName

  -- 2. Check if the number of declared arguments matches the expected parameters from the type signature.
  if length funcArgs /= length expectedParamTypes
    then Left $ "Type error in function '" ++ funcName ++ "': Declared type signature expects " ++ show (length expectedParamTypes) ++ " arguments, but " ++ show (length funcArgs) ++ " arguments are provided in the parameter list."
    else pure () -- Continue if arity matches

  -- 3. Create the local environment for the function's parameters using their EXPECTED types.
  let localEnvForBody = Map.fromList (zip funcArgs expectedParamTypes)

  -- 4. Infer the type of the function's body.
  inferredBodyType <- inferExprType globalEnv localEnvForBody funcBody

  -- 5. Compare the inferred body type with the declared return type.
  if inferredBodyType == expectedReturnType
    then Right () -- Function body type matches declared return type
    else Left $ "Type error in function '" ++ funcName ++ "': Declared return type is " ++ show expectedReturnType ++ " but inferred body type is " ++ show inferredBodyType

-- Helper to unpack a curried function type into a list of argument types and a final return type.
-- It also performs an arity check against the number of actual function arguments (`numArgs`).
unpackFunctionType :: Type -> Int -> String -> Either String ([Type], Type)
unpackFunctionType funcType numArgs funcName = go funcType numArgs []
  where
    -- Base case: we have unpacked all the arguments we expected (numArgs is 0).
    -- The `currentType` is the final return type.
    go currentType 0 accParams = Right (reverse accParams, currentType)

    -- Recursive step: If we still expect arguments (n > 0) and we have a function type...
    go (TArr argType restType) n accParams =
      -- ...unpack one argument type and recurse.
      go restType (n - 1) (argType : accParams)

    -- Error case: We still expect arguments (n > 0), but the type is not a function.
    -- This means the type signature is not a function or doesn't have enough arguments.
    go nonFuncType n _ = Left $ "Type error in '" ++ funcName ++ "': Type signature does not have enough arguments. Expected " ++ show numArgs ++ " but signature only provided " ++ show (numArgs - n) ++ ". Found non-function type: " ++ show nonFuncType

--------------------------------------------------------------------------------
-- 5. Top-Level Program Type Checker
--------------------------------------------------------------------------------

-- The main entry point for type checking the entire program
checkProgram :: [Function] -> Either String GlobalEnv
checkProgram funcs = do
  -- 1. Initialize the global environment with built-in function types.
  let initialGlobalEnv = Map.singleton "print" (TArr TString TUnit)

  -- 2. First Pass: Populate GlobalEnv with *declared* function signatures directly.
  --    The parser should have already ensured `funcTypeSignature` is filled and that
  --    all defined functions have a type signature.
  let userFuncSignatures = Map.fromList $ map (\f -> (funcName f, funcTypeSignature f)) funcs

  -- 3. Combine built-in and user-defined signatures into a complete global environment.
  let fullGlobalEnv = Map.union userFuncSignatures initialGlobalEnv

  -- 4. Second Pass: Now, with the complete `fullGlobalEnv` available,
  --    type-check the *bodies* of all user-defined functions.
  --    `mapM_` is used here because `checkFunctionDefinition` returns `Either String ()`,
  --    and we only care that all checks succeed (no `Left` results).
  mapM_ (checkFunctionDefinition fullGlobalEnv) funcs

  -- 5. If all checks pass, return the complete global function environment.
  return fullGlobalEnv
  