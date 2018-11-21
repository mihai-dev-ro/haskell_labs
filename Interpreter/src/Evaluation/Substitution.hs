module Evaluation.Substitution where

import Syntax.Expression
import Syntax.Grammar
import Syntax.Parser
import qualified Data.Set as Set

{-|
    Returns the list of free variables in an expression.
-}
freeVars :: Expression -> [String]

freeVars (Var x) = [x]

freeVars (Lambda x e) = 
    Set.toList $ Set.difference (Set.fromList $ freeVars e) (Set.fromList [x])

freeVars (Application e a) = 
    Set.toList $ Set.union (Set.fromList $ freeVars e) $
        (Set.fromList $ freeVars a)

freeVars (Definition name e) = freeVars e

{-|
    Performs the substitution of the free occurrences of a variable within
    an expression with another expression.
-}
subst :: String      -- ^ Variable
      -> Expression  -- ^ New expression
      -> Expression  -- ^ Existing expression
      -> Expression  -- ^ Resulting expression

subst x e existingExpr@(Var y)
    | y == x = e
    | y /= x = existingExpr

subst x e' existingExpr@(Lambda y e) 
    | y == x                       = existingExpr
    | not (y `elem` (freeVars e')) = Lambda { argName = y, body = subst x e' e}
    | otherwise = 
        let findZ y = if ((y ++ "#") `elem` ((freeVars e) ++ (freeVars e'))) 
                      then findZ (y ++ "#") 
                      else (y ++ "#") 
        in
            (\z -> Lambda { argName = z, body = subst x e' (subst y (Var z) e) }) $ (findZ y)

subst x e' (Application e a) = 
    Application { expr = subst x e' e, arg = subst x e' a}

subst x e' (Definition name e) =
    Definition { name = name, expr = subst x e' e }    

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
        newExpr = Var { name = "y" }
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
                  (runParser anyLambdaExpression "z")
        existingExpr = (
                        (\(Just (e, _)) -> e) <$> 
                        (runParser $ anyLambdaExpression) <$> 
                        ["\\x.x"]
                       )
    in  
        subst "x" newExpr <$> existingExpr
            

