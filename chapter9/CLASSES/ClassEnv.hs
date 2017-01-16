{-
 -  CLASSES/ClassEnv.hs
 -
 -  Reference implementation of the toy language CLASSES by Mitchell Wand.
 -  This module provides a representation for closures of first-class functions.
 -
 -  Author: Matthew A Johnson
 -}
module CLASSES.ClassEnv where

import                Common.Types

import                CLASSES.AST
import {-# SOURCE #-} CLASSES.Environment
import                CLASSES.Store

{- Purpose-specific synonyms for identifiers -}
type ClassName  = Id
type FieldName  = Id
type MethodName = Id

{- Environments for class and method lookups -}
type ClassEnv  = [(ClassName, Class)]
type MethodEnv = [(MethodName,Method)]

{- Data structure representation of an instance of a class -}
data Object = Object { objClass  :: ClassName
                     , objFields :: [Reference]
                     } deriving Show

{- Data structure representation of a class definition -}
data Class  = Class { classSuper  :: ClassName
                    , classFields :: [FieldName]
                    , methodEnv   :: MethodEnv
                    } deriving Eq

{- Memory allocation for a new instance of a class -}
newObject :: Store -> ClassEnv -> ClassName -> (Object,Store)
newObject σ φ cname = (Object cname refs,σ') where
    (refs,σ')  = foldr allocateRef ([],σ) fieldNames
    fieldNames = classFields (lookupClass φ cname)
    allocateRef f (locs,σ) = let (loc,σ') = newref undefined σ
                              in (loc:locs,σ')

{- Data structure representation of a method -}
data Method = Method { methodParams :: [Id]
                     , methodBody   :: Exp
                     , methodSuper  :: ClassName
                     , methodFields :: [FieldName]
                     } deriving Eq

{- Look up a method in a given class by name -}
findMethod :: ClassEnv -> ClassName -> MethodName -> Method
findMethod φ cname name = case lookup name menv of
    Nothing   -> reportMethodNotFound name
    Just mthd -> mthd
  where
    menv = methodEnv (lookupClass φ cname)

reportMethodNotFound = error . ("findMethod: unknown method " ++) . show

{- Join method environments so that derived methods override superclass methods -}
mergeMethodEnvs :: MethodEnv -> MethodEnv -> MethodEnv
mergeMethodEnvs superEnv newEnv = newEnv ++ superEnv

{- Build a method environment from an AST method declaration -}
methodDeclsToMenv :: [MethodDecl] -> ClassName -> [FieldName] -> MethodEnv
methodDeclsToMenv mdecls sname fields = map declToMethod mdecls where
    declToMethod (MethodDecl name params body) = (name,Method params body sname fields)

{- Place a new class definition at the head of the class environment -}
addClass :: ClassEnv -> ClassName -> Class -> ClassEnv
addClass φ cname cdef = (cname,cdef):φ

{- Returns the class definition for a given class name (if found in the environment) -}
lookupClass :: ClassEnv -> ClassName -> Class
lookupClass φ cname = case lookup cname φ of
                                 Nothing  -> reportUnknownClass cname
                                 Just cls -> cls

reportUnknownClass = error . ("lookupClass: unknown class "++) . show

{- Populate the class environment using all the AST class declarations -}
initClassEnv :: [ClassDecl] -> ClassEnv
initClassEnv decls = foldl initClassDecl [("object",baseclass)] decls where
    baseclass = Class "" [] [("initialize",Method [] SelfExp "" [])]

{- Builds a class definitions from an AST declaration of a class and adds it to the class environment -}
initClassDecl :: ClassEnv -> ClassDecl -> ClassEnv
initClassDecl φ (ClassDecl cname sname fields mdecls) = addClass φ cname (Class sname fnames menv)
  where
    fnames = appendFieldNames (classFields (lookupClass φ sname)) fields
    menv = mergeMethodEnvs (methodEnv (lookupClass φ sname)) (methodDeclsToMenv mdecls sname fnames)

{- Creates a combined list of all derived-class and super-class field names
   Note: super-class field names appear first so that their positions can be statically guaranteed.
   Note: super-class field names may be mangled so derived-class fields of the same name shadow them -}
appendFieldNames :: [FieldName] -> [FieldName] -> [FieldName]
appendFieldNames superFields newFields = appendFieldNamesAux superFields 0 where
    appendFieldNamesAux :: [FieldName] -> Int -> [FieldName]
    appendFieldNamesAux []     _ = newFields
    appendFieldNamesAux (s:ss) k = f : appendFieldNamesAux ss (k+1) where
        f = if s `elem` newFields then freshIdentifier k s else s

{- Computes a new mangled name for a super-class field -}
freshIdentifier :: Int -> Id -> Id
freshIdentifier k name = name ++ '%' : show k

