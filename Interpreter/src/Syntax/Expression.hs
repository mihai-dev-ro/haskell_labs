module Syntax.Expression where

import qualified Data.Map as M

data Expression = Var { name :: String } 
                | Lambda { argName :: String, body :: Expression }
                | Application { expr :: Expression, arg :: Expression }
                | Definition { name :: String, expr :: Expression }
                deriving (Eq)


instance Show Expression where
    show (Var { name = x }) = x
    show (Lambda x e) = "\\" ++ x ++ "." ++ (show e)
    show (Application e a) = "(" ++ (show e) ++ " " ++ (show a) ++ ")"
    show (Definition name e) = name ++ "=" ++ (show e) 


type Context = M.Map String Expression







