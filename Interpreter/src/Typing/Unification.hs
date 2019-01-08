module Typing.Unification where

import Typing.Type
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Except
import Data.Either
import Control.Arrow

{-
    A monad for solving unification constraints. It is composed of:
    * a state monad transformer, for maintaining the substitution
    * an exception monad, for signaling unification errors.
-}

type Unif = StateT Substitution (Except String)

runUnif :: Unif a -> Substitution -> Either String (a, Substitution)
runUnif ops subst = runExcept $ runStateT ops subst

{-|
    Obtains the end of the binding chain for the given type.
    The search ends when either of the following is reached:
    
    * an unbound type variable
    
    * a function type.
-}
chainEnd :: Type       -- ^ Type to look up
         -> Unif Type  -- ^ Chain end

chainEnd tv@(TypeVar v) = do
    subst <- get
    case M.lookup v subst of
        Nothing                 -> return tv
        Just ta@(Arrow t1 t2)   -> return ta
        Just tv'@(TypeVar v')   -> chainEnd tv'

chainEnd ta@(Arrow t1 t2) = do
    t1' <- chainEnd t1
    t2' <- chainEnd t2
    return (Arrow t1' t2')


{-
chainEnd ts@(TypeVar s) = StateT (
                            \subst -> case M.lookup s subst of
                                Nothing              -> return (ts, subst)                                
                                Just t@(Arrow t1 t2) -> return (t, subst)
                                Just t@(TypeVar s')  -> runStateT (chainEnd t) subst
                          )
-}

{-|
    Returns true if a type variable does NOT appear in the given type.
    The type variable is assumed FREE within the substitution.
-}
occCheck :: String     -- ^ Type variable to check for occurrence
         -> Type       -- ^ Type to look in
         -> Unif Bool  -- ^ True if the type variable does NOT occur
occCheck x tv@(TypeVar v) = do
    subst <- get
    case M.lookup x subst of
        Just t' -> 
            let err = "error. variable <<" ++ x ++ ">> is not free in the \
                    \ substitution: " ++ (show subst)
            in  throwError err
        Nothing -> 
            return False  -- TO ASK: how to avoid treating this branch
    tv' <- chainEnd tv
    case tv' of 
        TypeVar v' ->
            if x == v' 
            then return False
            else return True
        ta@(Arrow t1 t2) -> occCheck x ta 
     
occCheck x ta@(Arrow t1 t2) = do
    t1occ <- occCheck x t1
    t2occ <- occCheck x t2
    return (t1occ && t2occ)
    
{-|
    Unifies two type expressions.
-}
unify :: Type     -- ^ First type
      -> Type     -- ^ Second type
      -> Unif ()  -- ^ () if the types unify or an exception otherwise

unify t1 t2 = do
    t1' <- chainEnd t1
    t2' <- chainEnd t2
    unifyBase t1' t2'

unifyBase tv1@(TypeVar v1) tv2@(TypeVar v2) = do
    if v1 == v2 
    then return ()
    else do 
        modify $ M.insert v1 tv2
        return ()         

unifyBase tv1@(TypeVar v1) ta2@(Arrow t1 t2) = do
    tv1occ <- occCheck v1 ta2
    if tv1occ
    then do
        modify $ M.insert v1 ta2
        return ()                
    else do
        let err = "error. failed occurance check"
        throwError err

unifyBase ta1@(Arrow t1 t2) ta2@(Arrow t1' t2') = do
    unify t1 t1'
    unify t2 t2'
    return ()

unifyBase ta1@(Arrow t1 t2) tv2@(TypeVar v2) = unify tv2 ta1


{-|
    Applies the substitution to a type expression.
-}
applySubst :: Type       -- ^ Target type
           -> Unif Type  -- ^ Resulting type

applySubst tv@(TypeVar v) = do
    tv' <- chainEnd tv
    case tv' of 
        TypeVar v'          -> return tv'
        ta'@(Arrow t1' t2') -> applySubst ta'


applySubst ta@(Arrow t1 t2) = do
    t1' <- applySubst t1
    t2' <- applySubst t2
    return (Arrow t1' t2')










