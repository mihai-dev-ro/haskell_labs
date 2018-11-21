module Evaluation.Applicative where

import Syntax.Expression
import Evaluation.Substitution
import qualified Data.Map as M

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
