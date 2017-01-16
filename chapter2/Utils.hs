module Utils where

type Var = String
type Val = Int

class Env e where
	-- empty_env = ⌈0⌉
	empty_env :: e
	{- extend_env var val ⌈f⌉ = ⌈g⌉
	       where g(var₁) = v        if var₁ = var
		                   f(var₁)  otherwise -}
	extend_env :: Var -> Val -> e -> e
	-- apply_env ⌈f⌉ var = f(var)
	apply_env :: e -> Var -> Val

