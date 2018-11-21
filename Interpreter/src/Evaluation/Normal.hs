module Evaluation.Normal where

import Syntax.Expression
import Evaluation.Substitution
import qualified Data.Map as M

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