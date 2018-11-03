module Syntax.Expression where

data Expression = Var { varName :: String } 
                | Lambda { lambdaArg :: String, lambdaBody :: Expression }
                | Application { appExpr :: Expression, appArg :: Expression }
                | Definition { defName :: String, defExpr :: Expression }


instance Show Expression where
    show (Var { varName = x }) = x
    show (Lambda { lambdaArg = x, lambdaBody = e}) = "λ" ++ x ++ "." ++ (show e)
    show (Application { appExpr = e, appArg = a }) = "(" ++ (show e) ++ " " ++ (show a) ++ ")"
    show (Definition { defName = name, defExpr = e }) = name ++ "=" ++ (show e) 








