module Evaluation.Applicative where

import Syntax.Expression
import Evaluation.Substitution
import qualified Data.Map as M
import Control.Monad.State


{-|
  Small-step applicative-order evaluation of a given expression,
  within a given context.
-}
eval :: Expression             -- ^ Expression to be evaluated
  -> Context                -- ^ Context where the evaluation takes place
  -> (Expression, Context)  -- ^ Evaluation result, together with a possibly
                 --   enriched context, in case of definition

eval def@(Definition name e) context = (e, M.insert name e context)

eval var@(Var x) context = (M.findWithDefault var x context, context)

eval lambda@(Lambda x e) context = (lambda, context)

eval (Application var@(Var x) a) context = 
  let evalVar = fst $ (eval var context)
  in ((Application evalVar a), context)
eval (Application lambdaE@(Lambda x e) a) context = case a of
  lambdaA@(Lambda x' e')  -> (subst x lambdaA e, context)
  var@(Var x)             -> (subst x var e, context)
  app@(Application e' a') -> ((Application lambdaE (fst $ eval app context)), context)
eval e context = (e, context)


type Eval = State Context
evalM :: Expression -> Eval Expression

evalM def@(Definition name e) = state $ \s -> (e, M.insert name e s)

evalM var@(Var x) = do
  context <- get
  case M.lookup x context of
    Nothing -> do
      modify $ M.insert x var
      return var
    Just result -> return result

evalM lambda@(Lambda x e) = return lambda

evalM (Application var@(Var x) a) = do
  evalVar <- evalM var
  return (Application evalVar a)

evalM (Application lambdaE@(Lambda x e) a) = case a of
  lambdaA@(Lambda x' e')  -> return (subst x lambdaA e)
  var@(Var x)             -> return (subst x var e)
  app@(Application e' a') -> do
    evalApp <- evalM app
    return (Application lambdaE evalApp)

evalM e = return e