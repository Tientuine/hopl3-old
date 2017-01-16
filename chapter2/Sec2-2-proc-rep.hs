{-# LANGUAGE FlexibleInstances,TypeSynonymInstances #-}

module Sec_2_2_proc_rep where

type Var = String
type Val = Int

class Env e where
  -- empty_env = ⌈0⌉
  empty_env :: e
  -- extend_env var val ⌈f⌉ = ⌈g⌉  where g(var₁) = v if var₁ = var
  --                                               f(var₁) otherwise
  extend_env :: Var -> Val -> e -> e
  -- apply_env ⌈f⌉ var = f(var)
  apply_env :: e -> Var -> Val

report_no_binding_found searchVar = error ("apply_env: No binding for "++searchVar)

type ProcEnv = Var -> Val

instance Env ProcEnv where
  empty_env = \searchVar -> report_no_binding_found searchVar
  extend_env savedVar savedVal savedEnv = \searchVar -> if searchVar == savedVar
                                                          then savedVal
                                                          else apply_env savedEnv searchVar
  apply_env env searchVar = env searchVar

