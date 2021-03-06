module Evaluation.Big where

import Syntax.Expression
import qualified Data.Map as M
import qualified Data.List as L
import Evaluation.Normal
import Data.Tuple
import Data.Either
import Control.Arrow (first)

import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Except

{-|
    Big-step evaluation of a given expression, within a given context.
    The evaluation should stop when either the value is reached,
    or the expression cannot be reduced further.
    
    The first argument is the small-step evaluation function.
-}
evalBig :: (Expression -> Context -> IO (Expression, Context))  -- ^ Small-stepper
        -> Expression             -- ^ Expression to be evaluated
        -> Context                -- ^ Context where the evaluation takes place
        -> IO (Expression, Context)  -- ^ Evaluation result,
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

evalBig smallStepper e context = 
  let smallStepperM e = lift . StateT $ smallStepper e
  in do
    result <- runStateT (runExceptT $ evalBigM smallStepperM e) context
    return $ first (fromRight e) result

evalBigM :: (Expression -> Eval Expression)
         -> Expression
         -> Eval Expression
evalBigM smallStepper e = do
  e'<- smallStepper e
  if e == e'
    then return e
    else evalBigM smallStepper e'

                        
{-|
    Big-step evaluation of a list of expressions, starting with
    the given context and using it throughout the entire list,
    for propagating the encountered definitions.
    
    The first argument is the small-step evaluation function.
-}
evalList :: (Expression -> Context -> IO (Expression, Context))
         -> [Expression]
         -> Context
         -> IO ([Expression], Context)

{-}
evalList smallStepper es context = 
  let 
    fnAcc context e = swap $ evalBig smallStepper e context
  in 
    swap $ L.mapAccumL fnAcc context es 
-}

evalList smallStepper es context = 
  let smallStepperM e = lift . StateT $ smallStepper e 
  in do 
    result <- runStateT (runExceptT $ evalListM smallStepperM es) context
    return $ first (fromRight es) result

evalListM :: (Expression -> Eval Expression)
          -> [Expression]
          -> Eval [Expression]
-- evalListM smallStepperM es = mapM (evalBigM smallStepperM) es
evalListM = mapM . evalBigM

