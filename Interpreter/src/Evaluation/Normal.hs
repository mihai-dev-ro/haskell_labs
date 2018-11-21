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
eval def@(Definition name e) context = (e, M.insert name e context)

eval var@(Var x) context = (M.findWithDefault var x context, context)

eval lambda@(Lambda x e) context = (lambda, context)

eval app@(Application e a) context = case e of                                        
                    Var x       -> 
                      let evalVar = fst $ (eval e context)
                      in ((Application evalVar a), context)
                    Lambda x e' -> (subst x a e', context)
                    _           -> (app, context)


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

evalM app@(Application e a) = case e of
  Var x       -> do
    evalVar <- evalM e
    return (Application evalVar a)
  Lambda x e' -> return (subst x a e')
  _           -> return app
    
