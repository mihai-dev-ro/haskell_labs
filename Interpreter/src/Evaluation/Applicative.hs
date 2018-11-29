module Evaluation.Applicative where

import Syntax.Expression
import Evaluation.Substitution
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Identity
import Data.Either


{-|
  Small-step applicative-order evaluation of a given expression,
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

eval app@(Application lambdaE@(Lambda x e) a) context = case a of
  Lambda x' e'  -> (subst x a e, context)
  _             -> 
    let evalA = fst $ eval a context 
    in  ((Application lambdaE evalA), context) 

eval app@(Application e a) context = 
  let evalE = fst $ eval e context
  in ((Application evalE a), context)
-}

type Eval = ExceptT String (State Context)
evalM :: Expression -> Eval Expression

evalE :: Expression -> Context -> (Either String Expression, Context)
evalE e = runState $ runExceptT (evalM e)
eval e = applyToFirst (fromRight e) . evalE e 

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

evalM (Application lambdaE@(Lambda x e) a) = case a of
  Lambda x' e' -> lift $ return (subst x a e)
  _ -> evalM a >>= lift . return . Application lambdaE
    
evalM (Application e a) = do
  evalE <- evalM e 
  lift $ return (Application evalE a)
