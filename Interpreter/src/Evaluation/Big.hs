module Evaluation.Big where

import Syntax.Expression
import qualified Data.Map as M
import Evaluation.Normal

{-|
    Big-step evaluation of a given expression, within a given context.
    The evaluation should stop when either the value is reached,
    or the expression cannot be reduced further.
    
    The first argument is the small-step evaluation function.
-}
evalBig :: (Expression -> Context -> (Expression, Context))  -- ^ Small-stepper
        -> Expression             -- ^ Expression to be evaluated
        -> Context                -- ^ Context where the evaluation takes place
        -> (Expression, Context)  -- ^ Evaluation result,
                                  --   together with a possibly enriched context
                                  --   in case of definition

evalBig smallStepper var@(Var x) context = case M.lookup x context of 
                                            Nothing -> (var, context)
                                            Just e'  -> evalBig smallStepper e' context

evalBig smallStepper lambda@(Lambda x e) context = (lambda, context)

evalBig smallStepper def@(Definition name e) context = smallStepper def context

evalBig smallStepper app@(Application e a) context = case e of 
  eVar@(Var x') -> case M.lookup x' context of 
                    Nothing -> (app, context)
                    Just e' -> evalBig smallStepper (Application e' a) context
  eLambda@(Lambda x' e')      -> 
    let (appEval, contextEval) = smallStepper app context
    in evalBig smallStepper appEval contextEval
  
  eApp@(Application e' a') -> 
    let (eApp', context') = smallStepper eApp context
    in evalBig smallStepper (Application eApp' a) context'

                        
{-|
    Big-step evaluation of a list of expressions, starting with
    the given context and using it throughout the entire list,
    for propagating the encountered definitions.
    
    The first argument is the small-step evaluation function.
-}
evalList :: (Expression -> Context -> (Expression, Context))
         -> [Expression]
         -> Context
         -> ([Expression], Context)
evalList smallStepper exprList context =
  let 
    foldlFn (es, esContext) e =  mergeExprTuples (evalBig smallStepper e esContext) (es, esContext) 
  in applyToFirst reverse $ foldl foldlFn ([], context) exprList


mergeExprTuples :: (a,b) -> ([a],b) -> ([a], b)
mergeExprTuples (x,y) (xs,_) = (x:xs, y) 

{-|
    Applies a function to the first component of a pair.
-}
applyToFirst :: (a -> b) -> (a, c) -> (b, c)
applyToFirst f (x, y) = (f x, y)
