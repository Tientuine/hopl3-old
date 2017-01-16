{-
 -  CLASSES/Interp.hs
 -
 -  Reference implementation of the toy language CLASSES by Mitchell
 -  Wand. This module provides the core interpreter implementation.
 -
 -  Author: Matthew A Johnson
 -}
module CLASSES.Interp (runInterp,interp,prelude,Answer) where

import           Data.List
import           System.Environment
import Debug.Trace
import           Common.Types
import           CLASSES.AST
import           CLASSES.ClassEnv
import           CLASSES.Closure
import           CLASSES.Environment
import           CLASSES.Parser
import           CLASSES.Prelude
import           CLASSES.Store
import           CLASSES.Val

type Closure = Proc
type Answer  = (ExpVal,Store)
type Answers = ([ExpVal],Store)

getval :: Answer -> ExpVal
getval = fst

getvals :: Answers -> [ExpVal]
getvals = fst

getstore :: (v,Store) -> Store
getstore = snd

runInterp :: IO ()
runInterp = do
    args <- getArgs
    if null args
        then putStrLn "IMPLICIT_REFS: Missing source file name"
        else do prog <- readFile $ head args
                print $ interp prog prelude
                return ()

{- Shim for pre-interpreting the prelude definitions -}

compileOne :: Def -> (Env,Store) -> (Env,Store)
compileOne (name,src) (ρ,σ) = (extendEnv name (RefVal loc) ρ,σ₁)
  where
    (loc,σ₁) = newref (interp src (ρ,σ)) σ

prelude :: (Env,Store)
prelude = foldr compileOne (emptyEnv,freestore 250 undefined) preludeSrc

{- Top-level interpreter -}

interp :: Source -> (Env,Store) -> ExpVal
interp pgm (ρ,σ) = valueOfProgram (parser pgm) ρ σ

valueOfProgram :: Pgm -> Env -> Store -> ExpVal
valueOfProgram (Pgm decls exp) ρ σ = getval $ valueOf exp ρ φ σ
  where
    φ = initClassEnv decls

valueOf :: Exp -> Env -> ClassEnv -> Store -> Answer

valueOf (ConstExp n) ρ φ σ = (NumVal n,σ)

valueOf (VarExp x) ρ φ σ = (deref loc σ,σ)
  where
    RefVal loc = applyEnv ρ x

valueOf (IsZeroExp rand) ρ φ σ = (BoolVal (n == 0),σ₁)
  where
    (NumVal n,σ₁) = valueOf rand ρ φ σ

valueOf (DiffExp rand₁ rand₂) ρ φ σ = (NumVal (n₁ - n₂),σ₂)
  where
    (NumVal n₁,σ₁) = valueOf rand₁ ρ φ σ
    (NumVal n₂,σ₂) = valueOf rand₂ ρ φ σ₁

valueOf (LetExp xs rhss body) ρ φ σ = valueOf body ρ' φ σ₂
  where
    ρ' = extendEnv' xs (map RefVal locs) ρ
    (locs,σ₂) = newrefs vals σ₁
    (vals,σ₁) = valueOfExps rhss ρ φ σ

valueOf (IfExp test conseq altern) ρ φ σ
    | res == True    = valueOf conseq ρ φ σ₁
    | otherwise      = valueOf altern ρ φ σ₁
  where
    (BoolVal res,σ₁) = valueOf test ρ φ σ

valueOf (ProcExp param body) ρ φ σ = (ProcVal (Proc param body ρ),σ)

valueOf (CallExp rator rand) ρ φ σ = applyProcedure proc arg φ σ₂
  where
    (ProcVal proc,σ₁) = valueOf rator ρ φ σ
    (arg,σ₂)          = valueOf rand ρ φ σ₁

valueOf (IsNegExp rand) ρ φ σ = (BoolVal (n < 0),σ₁)
  where
    (NumVal n,σ₁) = valueOf rand ρ φ σ

valueOf (LetrecExp pname bvar pbody body) ρ φ σ = valueOf body ρ' φ σ₂
  where
    ρ'       = extendEnv pname (RefVal loc) ρ
    (loc,σ₁) = newref undefined σ
    σ₂       = setref loc (ProcVal (Proc bvar pbody ρ')) σ₁

valueOf (AssignExp var rhs) ρ φ σ = (NumVal 42,σ₂)
  where
    (rval,σ₁)  = valueOf rhs ρ φ σ
    RefVal loc = applyEnv ρ var
    σ₂         = setref loc rval σ₁

valueOf (BeginExp exps) ρ φ σ =
    foldl' (\ans exp -> valueOf exp ρ φ (getstore ans)) (NumVal 42,σ) exps

valueOf EmptyExp ρ φ σ = (ListVal [],σ)

valueOf (IsNullExp rand) ρ φ σ = (BoolVal (null vs),σ₁)
  where
    (ListVal vs,σ₁) = valueOf rand ρ φ σ

valueOf (ConsExp rand₁ rand₂) ρ φ σ = (ListVal (v:vs),σ₂)
  where
    (v,σ₁)          = valueOf rand₁ ρ φ σ
    (ListVal vs,σ₂) = valueOf rand₂ ρ φ σ₁

valueOf (CarExp rand) ρ φ σ = (head vs,σ₁)
  where
    (ListVal vs,σ₁) = valueOf rand ρ φ σ

valueOf (CdrExp rand) ρ φ σ = (ListVal (tail vs),σ₁)
  where
    (ListVal vs,σ₁) = valueOf rand ρ φ σ

valueOf (ListExp rands) ρ φ σ = (ListVal vals,σ₁)
  where
    (vals,σ₁) = valueOfExps rands ρ φ σ

valueOf (NewObjExp cname rands) ρ φ σ = (ObjVal obj,σ₃)
  where
    (args,σ₁) = valueOfExps rands ρ φ σ
    (obj ,σ₂) = newObject σ₁ φ cname
    (_,σ₃)    = applyMethod (findMethod φ cname "initialize") obj args φ σ₂

valueOf SelfExp ρ φ σ = (val,σ)
  where
    RefVal loc = applyEnv ρ "%self"
    val = deref loc σ

valueOf (MethodCallExp exp name rands) ρ φ σ = applyMethod method obj args φ σ₂
  where
    (ObjVal obj,σ₁) = valueOf exp ρ φ σ
    (args,σ₂) = valueOfExps rands ρ φ σ₁
    method    = findMethod φ (objClass obj) name

valueOf (SuperCallExp name rands) ρ φ σ = applyMethod method obj args φ σ₁
  where
    RefVal selfLoc = applyEnv ρ "%self"
    Label super = applyEnv ρ "%super"
    ObjVal obj = deref selfLoc σ
    (args, σ₁) = valueOfExps rands ρ φ σ
    method = findMethod φ super name

valueOf (InstanceofExp exp name) ρ φ σ = (BoolVal (isInstance objcls),σ₁)
  where
    (ObjVal obj,σ₁) = valueOf exp ρ φ σ
    objcls = lookupClass φ $ objClass obj
    target = lookupClass φ name
    isInstance cls | cls == target = True
                   | cls == lookupClass φ "object" = False
                   | otherwise = isInstance $ lookupClass φ (classSuper cls)

{--- Auxiliary functions ---}

applyMethod :: Method -> Object -> [ExpVal] -> ClassEnv -> Store -> Answer
applyMethod (Method params body sname fields) self args φ σ = valueOf body ρ' φ σ₂
  where
    (locs,   σ₁) = newrefs args σ
    (selfloc,σ₂) = newref (ObjVal self) σ₁
    ρ' = extendEnv' params (map RefVal locs)
            (extendEnv "%self" (RefVal selfloc)
                (extendEnv "%super" (Label sname)
                    (extendEnv' fields (map RefVal $ objFields self) emptyEnv)))

valueOfExps :: [Exp] -> Env -> ClassEnv -> Store -> Answers
valueOfExps exps ρ φ σ = foldr accumAnswers ([],σ) exps
  where
    accumAnswers exp (vs,σ) = let (v,σ') = valueOf exp ρ φ σ in (v:vs,σ')

applyProcedure :: Closure -> ExpVal -> ClassEnv -> Store -> Answer
applyProcedure proc arg φ σ = valueOf body (extendEnv param (RefVal loc) savedEnv) φ σ₁
  where
    Proc param body savedEnv = proc
    (loc,σ₁) = newref arg σ

