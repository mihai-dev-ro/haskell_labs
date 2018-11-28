module Evaluation.Big where

import Syntax.Expression
import qualified Data.Map as M
import qualified Data.List as L
import Evaluation.Normal
import Data.Tuple

import Control.Monad.State

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

{-
evalBig smallStepper def@(Definition name e) context = smallStepper def context

evalBig smallStepper e context =
  let e' = fst $ smallStepper e context
  in if e == e' 
      then (e, context)
      else evalBig smallStepper e' context 
-}

evalBig smallStepper e = runState $ evalBigM smallStepper e


evalBigM :: (Expression -> Context -> (Expression, Context))
         -> Expression
         -> Eval Expression
evalBigM smallStepper e = do
  context <- get
  (e', context') <- return $ smallStepper e context
  put context'
  if e == e'
    then return e
    else evalBigM smallStepper e'

                        
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

{-}
evalList smallStepper es context = 
  let 
    fnAcc context e = swap $ evalBig smallStepper e context
  in 
    swap $ L.mapAccumL fnAcc context es 
-}

evalList smallStepper es = runState $ evalListM smallStepper es

evalListM :: (Expression -> Context -> (Expression, Context))
          -> [Expression]
          -> Eval [Expression]
evalListM smallStepper es = mapM (evalBigM smallStepper) es