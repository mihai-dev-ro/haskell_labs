module Main where

import Syntax.Grammar (parseProgram)

{-|
main = readFile "prog.txt" 
	 >>= putStr
-}

main :: IO ()
main = readFile "prog.txt"
     >>= maybe (putStrLn "Syntax error!") (mapM_ print) . parseProgram
