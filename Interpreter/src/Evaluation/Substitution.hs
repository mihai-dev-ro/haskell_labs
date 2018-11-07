module Evaluation.Substitution where

import Syntax.Expression
import Syntax.Grammar
import Syntax.Parser
import qualified Data.Set as Set

{-|
    Returns the list of free variables in an expression.
-}
freeVars :: Expression -> [String]
freeVars (Var { varName = x }) = [x]
freeVars (Lambda { lambdaArg = x, lambdaBody = e }) = 
    Set.toList $ Set.difference (Set.fromList $ freeVars e) (Set.fromList [x])
freeVars (Application { appExpr = e, appArg = a }) = 
    Set.toList $ Set.union (Set.fromList $ freeVars e) $
        (Set.fromList $ freeVars a)
freeVars (Definition { defName = name, defExpr = e }) = freeVars e

{-|
    Performs the substitution of the free occurrences of a variable within
    an expression with another expression.
-}
subst :: String      -- ^ Variable
      -> Expression  -- ^ New expression
      -> Expression  -- ^ Existing expression
      -> Expression  -- ^ Resulting expression
subst x e existingExpr@(Var { varName = y })
    | y == x = e
    | y /= x = existingExpr

subst x e' existingExpr@(Lambda { lambdaArg = y, lambdaBody = e }) 
    | y == x                       = existingExpr
    | not (y `elem` (freeVars e')) = Lambda { lambdaArg = y, lambdaBody = subst x e' e}
    | otherwise = 
        let findZ y = if ((y ++ "#") `elem` ((freeVars e) ++ (freeVars e'))) 
                      then findZ (y ++ "#") 
                      else (y ++ "#") 
        in
            (\z -> Lambda { lambdaArg = z, lambdaBody = subst x e' e }) $ (findZ y)

subst x e' (Application { appExpr = e, appArg = a }) = 
    Application { appExpr = subst x e' e, appArg = subst x e' a}

subst x e' (Definition { defName = name, defExpr = e }) =
    Definition { defName = name, defExpr = subst x e' e }    

{-|
	Testing purposes
-}
testTreeVars = (\(Just (e, "")) -> freeVars e) <$> 
                   (runParser $ anyLambdaExpression) <$> 
                   ["x", "\\x.x", "\\x.y", "(x y)", 
                    "((x y) z)", "(\\x.x \\y.y)", 
                    "(\\x.x x)", "\\x.(x x)"]

testSubstVar = 
    let
        newExpr = Var { varName = "y" }
        existingExpr = (
                        (\(Just (e, _)) -> e) <$> 
                        (runParser $ anyLambdaExpression) <$> 
                        ["x", "z"]
                       )
    in
        subst "x" newExpr <$> existingExpr
            

testSubstLambda = 
    let 
        newExpr = (\(Just (e, _)) -> e) $ 
                  (runParser anyLambdaExpression "((x y) z)")
        existingExpr = (
                        (\(Just (e, _)) -> e) <$> 
                        (runParser $ anyLambdaExpression) <$> 
                        ["x", "\\z.\\z.x", "t"]
                       )
    in  
        subst "x" newExpr <$> existingExpr
            

