module Evaluation.Normal where

import Syntax.Expression
import Evaluation.Substitution
import qualified Data.Map as M
import Control.Monad.State

{-|
  Small-step normal-order evaluation of a given expression,
  within a given context.
-}
eval :: Expression             -- ^ Expression to be evaluated
   -> Context                -- ^ Context where the evaluation takes place
   -> (Expression, Context)  -- ^ Evaluation result, together with a possibly
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

eval = runState . evalM

type Eval = State Context
evalM :: Expression -> Eval Expression

evalM def@(Definition name e) = do
  context <- get
  modify $ M.insert name e
  return e

evalM var@(Var x) = do
  evalX <- gets $ M.findWithDefault var x
  return evalX

evalM lambda@(Lambda x e) = return lambda

evalM app@(Application e a) = case e of
  Lambda x e' -> return (subst x a e')
  _           -> do
    evalVar <- evalM e
    return (Application evalVar a)  
