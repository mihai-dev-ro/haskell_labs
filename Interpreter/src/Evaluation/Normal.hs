module Evaluation.Normal where

import Syntax.Expression
import Evaluation.Substitution
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Identity
import Data.Either

{-|
  Small-step normal-order evaluation of a given expression,
  within a given context.
-}
eval :: Expression             -- ^ Expression to be evaluated
   -> Context                -- ^ Context where the evaluation takes place
   -> IO (Expression, Context)  -- ^ Evaluation result, together with a possibly
                 --   enriched context, in case of definition
{-
eval def@(Definition name e) context = (e, M.insert name e context)

eval var@(Var x) context = (M.findWithDefault var x context, context)

eval lambda@(Lambda x e) context = (lambda, context)

eval app@(Application e a) context = case e of                                        
                    Lambda x e' -> (subst x a e', context)
                    _           -> let evalVar = fst $ (eval e context)
                                   in ((Application evalVar a), context)
-}




{- 
  Solution 1. Where Exception is treated outside of State 
  ------------------------------------------------------------------

type Eval = StateT Context (Except String)
evalM :: Expression -> Eval Expression

evalE :: Expression -> Context -> Either String (Expression, Context)
evalE e = runExcept . runStateT (evalM e) 
eval e context = fromRight (e, context) (evalE e context)
-}


{- 
  Solution 2. Where State is dealing with exception-aware expressions
  ------------------------------------------------------------------- 
-}

type Eval = ExceptT String (StateT Context IO)
evalM :: Expression -> Eval Expression

evalE :: Expression -> Context -> IO (Either String Expression, Context)
evalE e = runStateT . runExceptT $ evalM e
eval e context =  do 
                    result <- evalE e context
                    return $ applyToFirst (fromRight e) result


applyToFirst :: (a -> b) -> (a, c) -> (b, c)
applyToFirst f (x, y) = (f x, y)


evalM def@(Definition name e) = do
  modify $ M.insert name e
  lift $ return e

evalM var@(Var x) = do
  context <- get
  case M.lookup x context of
    Just e -> lift $ return e
    Nothing -> throwError $ "error. variable " ++ (show x) ++ "not found"

evalM lambda@(Lambda x e) = lift $ return lambda

evalM app@(Application e a) = case e of
  Lambda x e' -> lift $ return (subst x a e')
  _           -> do
    evalVar <- evalM e
    lift $ return (Application evalVar a)  
