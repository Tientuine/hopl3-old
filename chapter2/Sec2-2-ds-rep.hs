module Sec_2_2_ds_rep where

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

instance Env DSEnv where
  empty_env = EmptyEnv
  extend_env var val env = ExtendEnv var val env
  apply_env EmptyEnv searchVar = report_no_binding_found searchVar
  apply_env (ExtendEnv savedVar savedVal savedEnv) | searchVar == savedVar = savedVal
                                                   | otherwise             = apply_env savedEnv searchVar

