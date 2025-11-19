{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

module Inference where

import AST
import Types
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Identity
import Data.List (nub)
import Control.Monad
import Data.Functor ((<&>))

-- =========================================================================
-- 1. SUBSTITUTABLE CLASS (The Patcher and Scanner)
-- =========================================================================

class Substitutable a where
  apply :: Subst -> a -> a
  ftv   :: a -> Set.Set String

instance Substitutable Type where
  apply s t@(TVar a) = Map.findWithDefault t a s
  apply _  TInt      = TInt
  apply _  TString   = TString
  apply _  TBool     = TBool
  apply _  TUnit     = TUnit
  apply s (TList t)  = TList (apply s t)
  apply s (TArr t1 t2) = TArr (apply s t1) (apply s t2)
  apply s (THtml t)  = THtml (apply s t)
  apply _ t = t 

  ftv (TVar a)      = Set.singleton a
  ftv TInt          = Set.empty
  ftv TString       = Set.empty
  ftv TBool         = Set.empty
  ftv TUnit         = Set.empty
  ftv (TList t)     = ftv t
  ftv (TArr t1 t2)  = ftv t1 `Set.union` ftv t2
  ftv (THtml t)     = ftv t
  ftv _             = Set.empty

instance Substitutable Scheme where
  apply s (Forall vars t) =
    let s' = foldr Map.delete s vars
    in Forall vars (apply s' t)

  ftv (Forall vars t) =
    ftv t `Set.difference` Set.fromList vars

-- FIX: Explicitly define instance for lists of Type to resolve GHC ambiguity
instance Substitutable [Type] where
    apply s = map (apply s)
    ftv   l = foldr (Set.union . ftv) Set.empty l

instance Substitutable TypeEnv where
  apply s = Map.map (apply s)
  ftv env = ftv (Map.elems env) -- This now correctly calls the [Type] instance

-- =========================================================================
-- 2. MONAD & UNIFICATION (The Control System and Solver)
-- =========================================================================

data InferState = InferState
  { count :: Int
  }

type Infer a = ExceptT String (StateT InferState Identity) a

runInfer :: Infer a -> Either String a
runInfer inf = fst $ runIdentity $ runStateT (runExceptT inf) initialState
  where
    initialState = InferState { count = 0 }

freshTVar :: Infer Type
freshTVar = do
  s@InferState{count} <- get
  let name = "t" ++ show count
  put s { count = count + 1 }
  return (TVar name)

composeSubst :: Subst -> Subst -> Subst
s1 `composeSubst` s2 = Map.map (apply s1) s2 `Map.union` s1

bind :: String -> Type -> Either String Subst
bind a t
  | t == TVar a        = Right Map.empty
  | a `Set.member` ftv t = Left $ "Occurs check failed: infinite type " ++ a ++ " = " ++ show t
  | otherwise          = Right (Map.singleton a t)

unify :: Type -> Type -> Either String Subst
unify t1 t2 | t1 == t2 = Right Map.empty
unify (TVar a) t = bind a t
unify t (TVar a) = bind a t
unify (TArr t1 t2) (TArr t3 t4) = do
  s1 <- unify t1 t3
  s2 <- unify (apply s1 t2) (apply s1 t4)
  return (s2 `composeSubst` s1)
unify (TList t1) (TList t2) = unify t1 t2
unify (THtml t1) (THtml t2) = unify t1 t2
unify t1 t2 = Left $ "Type mismatch: Cannot unify " ++ show t1 ++ " with " ++ show t2

-- =========================================================================
-- 3. POLYMORPHISM HELPERS
-- =========================================================================

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Forall vars t
  where
    vars = Set.toList $ ftv t `Set.difference` ftv env

instantiate :: Scheme -> Infer Type
instantiate (Forall vars t) = do
  freshVars <- mapM (const freshTVar) vars
  let s = Map.fromList (zip vars freshVars)
  return $ apply s t

-- =========================================================================
-- 4. CORE INFERENCE (The AST Traversal)
-- =========================================================================

inferExpr :: GlobalEnv -> TypeEnv -> Expr -> Infer (Subst, Type)
inferExpr globalEnv env expr = case expr of

  LitInt _    -> return (Map.empty, TInt)
  LitString _ -> return (Map.empty, TString)
  LitBool _   -> return (Map.empty, TBool)
  LitUnit     -> return (Map.empty, TUnit)

  Var name -> case Map.lookup name env of
    Just t -> return (Map.empty, t)
    Nothing -> case Map.lookup name globalEnv of
      Just scheme -> do
        t <- instantiate scheme
        return (Map.empty, t)
      Nothing -> throwError $ "Undeclared variable or function: " ++ name
  
  Apply e1 e2 -> do
    tv <- freshTVar
    (s1, t1) <- inferExpr globalEnv env e1
    (s2, t2) <- inferExpr globalEnv (apply s1 env) e2
    
    s3 <- liftEither $ unify (apply s2 t1) (TArr t2 tv)
    
    let s_total = s3 `composeSubst` s2 `composeSubst` s1
    return (s_total, apply s_total tv)
  
  LitList elements -> do
    tv <- freshTVar
    if null elements
      then return (Map.empty, TList tv)
      else do
        (s1, t1) <- inferExpr globalEnv env (head elements)
        
        (s_rest, _) <- foldM (\(current_s, current_t) e -> do
                               (s_e, t_e) <- inferExpr globalEnv (apply current_s env) e
                               s_u <- liftEither $ unify (apply s_e t1) (apply s_e t_e)
                               return (s_u `composeSubst` s_e `composeSubst` current_s, apply s_u t_e)
                          ) (s1, t1) (tail elements)
                          
        s_final <- liftEither $ unify (apply s_rest t1) tv
        let s_total = s_final `composeSubst` s_rest
        
        return (s_total, TList (apply s_total t1))

  BinOp op e1 e2 -> inferBinOp globalEnv env op e1 e2
  Concat e1 e2 -> inferConcat globalEnv env e1 e2

  UnOp Not e1 -> do
    (s1, t1) <- inferExpr globalEnv env e1
    s2 <- liftEither $ unify t1 TBool
    let s_total = s2 `composeSubst` s1
    return (s_total, TBool)

  IfThenElse cond thenE elseE -> do
    (s1, t_cond) <- inferExpr globalEnv env cond
    s_cond <- liftEither $ unify t_cond TBool
    
    env' <- pure $ apply s_cond env
    (s2, t_then) <- inferExpr globalEnv env' thenE
    (s3, t_else) <- inferExpr globalEnv (apply s2 env') elseE
    
    s_branch <- liftEither $ unify (apply s3 t_then) t_else
    
    let s_total = s_branch `composeSubst` s3 `composeSubst` s2 `composeSubst` s_cond `composeSubst` s1
    return (s_total, apply s_total t_else)

  Block exprs -> case reverse exprs of
    [] -> return (Map.empty, TUnit)
    (lastE:restE) -> do
        (s_rest, _) <- foldM (\(s_acc, _) e -> inferExpr globalEnv (apply s_acc env) e) (Map.empty, TUnit) (reverse restE)
        (s_last, t_last) <- inferExpr globalEnv (apply s_rest env) lastE
        let s_total = s_last `composeSubst` s_rest
        return (s_total, apply s_total t_last)
        
  DoBlock exprs -> do
    (s, _) <- foldM (\(s_acc, _) e -> inferExpr globalEnv (apply s_acc env) e) (Map.empty, TUnit) exprs
    return (s, TUnit)

  OpAsFunction op -> do
    t_arg1 <- freshTVar
    t_arg2 <- freshTVar
    t_res <- freshTVar

    t_func <- case op of
      Add -> unifyOp TInt TInt TInt t_arg1 t_arg2 t_res
      Sub -> unifyOp TInt TInt TInt t_arg1 t_arg2 t_res
      Mul -> unifyOp TInt TInt TInt t_arg1 t_arg2 t_res
      Div -> unifyOp TInt TInt TInt t_arg1 t_arg2 t_res
      And -> unifyOp TBool TBool TBool t_arg1 t_arg2 t_res
      Or  -> unifyOp TBool TBool TBool t_arg1 t_arg2 t_res
      Eq  -> unifyOp t_arg1 t_arg2 TBool t_arg1 t_arg2 t_res 
      Neq -> unifyOp t_arg1 t_arg2 TBool t_arg1 t_arg2 t_res
      Lt  -> unifyOp TInt TInt TBool t_arg1 t_arg2 t_res
      Gt  -> unifyOp TInt TInt TBool t_arg1 t_arg2 t_res
      Le  -> unifyOp TInt TInt TBool t_arg1 t_arg2 t_res
      Ge  -> unifyOp TInt TInt TBool t_arg1 t_arg2 t_res

    return (Map.empty, t_func)

unifyOp :: Type -> Type -> Type -> Type -> Type -> Type -> Infer Type
unifyOp t_a t_b t_r t_arg1 t_arg2 t_res = do
    s1 <- liftEither $ unify t_arg1 t_a
    s2 <- liftEither $ unify (apply s1 t_arg2) (apply s1 t_b)
    s3 <- liftEither $ unify (apply s2 t_res) (apply s2 t_r)
    let s_total = s3 `composeSubst` s2 `composeSubst` s1
    return (apply s_total (TArr t_arg1 (TArr t_arg2 t_res)))


inferBinOp :: GlobalEnv -> TypeEnv -> BinOperator -> Expr -> Expr -> Infer (Subst, Type)
inferBinOp globalEnv env op e1 e2 = do
  (s1, t1) <- inferExpr globalEnv env e1
  (s2, t2) <- inferExpr globalEnv (apply s1 env) e2

  t_res <- case op of
    Add -> unifyAll s2 t1 t2 TInt TInt TInt
    Sub -> unifyAll s2 t1 t2 TInt TInt TInt
    Mul -> unifyAll s2 t1 t2 TInt TInt TInt
    Div -> unifyAll s2 t1 t2 TInt TInt TInt
    And -> unifyAll s2 t1 t2 TBool TBool TBool
    Or  -> unifyAll s2 t1 t2 TBool TBool TBool
    Lt  -> unifyAll s2 t1 t2 TInt TInt TBool
    Gt  -> unifyAll s2 t1 t2 TInt TInt TBool
    Le  -> unifyAll s2 t1 t2 TInt TInt TBool
    Ge  -> unifyAll s2 t1 t2 TInt TInt TBool
    Eq  -> unifyPolyEq s2 t1 t2
    Neq -> unifyPolyEq s2 t1 t2

  let s_final = s2 `composeSubst` s1
  return (s_final, apply s_final t_res)

  where
    unifyAll :: Subst -> Type -> Type -> Type -> Type -> Type -> Infer Type
    unifyAll s_arg2 t_expr1 t_expr2 t_in1 t_in2 t_out = do
      s_in1 <- liftEither $ unify (apply s_arg2 t_expr1) t_in1
      s_in2 <- liftEither $ unify (apply s_in1 (apply s_arg2 t_expr2)) t_in2
      return (apply s_in2 t_out)

    unifyPolyEq :: Subst -> Type -> Type -> Infer Type
    unifyPolyEq s_arg2 t_a t_b = do
      s_eq <- liftEither $ unify (apply s_arg2 t_a) (apply s_arg2 t_b)
      return TBool


inferConcat :: GlobalEnv -> TypeEnv -> Expr -> Expr -> Infer (Subst, Type)
inferConcat globalEnv env e1 e2 = do
  (s1, t1) <- inferExpr globalEnv env e1
  (s2, t2) <- inferExpr globalEnv (apply s1 env) e2
  
  s3 <- liftEither $ unify (apply s2 t1) TString
  s4 <- liftEither $ unify (apply s3 t2) TString
  
  let s_total = s4 `composeSubst` s3 `composeSubst` s2 `composeSubst` s1
  return (s_total, TString)
