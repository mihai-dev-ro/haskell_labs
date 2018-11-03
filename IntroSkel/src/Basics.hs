module Basics where  -- (10p)

import Data.Char
import qualified Data.Map as Map
import qualified Data.Set as Set

{-
    1. (1p)
    Implement the 'reverse' function, which returns a given list
    in reverse order, using explicit recursion.
    Do NOT employ any higher-order functions.
-}

reverseRec1 :: [a] -> [a]
reverseRec1 [] = []
reverseRec1 (x:xs) = reverseRec1 xs ++ [x] 


{-
    2. (1p)
    Same as (1), but change the direction on which the output is built.
    For example, if (1) built the list on return from recursion,
    you should now built the list when recursing forward.
-}

reverseRec2 :: [a] -> [a]
reverseRec2 [] = []
reverseRec2 x = 
    let loop acc (y:ys) = if length ys == 0
                            then (y:acc)
                            else loop (y:acc) ys
    in 
        loop [] x

{-
    3. (1.5p)
    Same as (1), but use a higher-order function instead.
    Do NOT employ explicit recursion.
    Make sure that your solution faithfully reflects the process from (1).
-}

reverseHO1 :: [a] -> [a]
reverseHO1 = foldr (\x acc -> acc ++ [x]) []

{-
    4. (1.5p)
    Same as (2), but use a higher-order function instead.
    Do NOT employ explicit recursion.
    Make sure that your solution faithfully reflects the process from (2).
-}

reverseHO2 :: [a] -> [a]
reverseHO2 = foldl (\acc x -> x : acc) []

{-
    5. (1p)
    Implement the power set function, which returns the set of all subsets
    of a set, using explicit recursion.
-}

powerSetRec :: [a] -> [[a]]
powerSetRec [] = [[]]
powerSetRec (x:xs) = [([x] ++ sbs) | sbs <- powerSetRec xs] ++ 
                     (powerSetRec xs) 


{-
    6. (1.5p)
    Same as (5), but use higher-order functions instead.
    Do NOT employ explicit recursion.
    Make sure that your solution faithfully reflects the process from (5).
-}

powerSetHO :: [a] -> [[a]]
powerSetHO = 
    let 
        buildSubset x acc = (map (x:) acc) ++ acc
    in 
        foldr buildSubset [[]]

{-
    7. (0.5p)
    Compute the cartesian product of two lists, using list comprehensions.
-}

cartesian2 :: [a] -> [b] -> [(a, b)]
cartesian2 x y = [ (elX, elY) | elX <- x, elY <- y]

{-
    8. (2p)
    Similar to (7), but extend to any number of lists.
-}

cartesian :: [[a]] -> [[a]]
cartesian = 
    let 
        productBuilder acc x = [ elAcc ++ [elX] | elAcc <- acc, elX <- x]
    in 
        foldl productBuilder [[]]

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let xLeft = quicksort [y | y <- xs, y < x] 
        xRight = quicksort [y | y <- xs, y >= x]
    in 
        xLeft ++ [x] ++ xRight

quicksort2 :: (Ord a) => [a] -> [a]
quicksort2 [] = []
quicksort2 (x:xs) = 
    let xLeft = quicksort2 . filter (<x) $ xs  
        xRight = quicksort2 . filter (>=x) $ xs
    in 
        xLeft ++ [x] ++ xRight

encode :: Int -> String -> String
encode shift = map $ chr . (+shift) . ord

decode :: Int -> String -> String
decode shift = encode $ negate shift 

data Point = Point Float Float deriving (Show)

type Name = String
type PhoneNumber = String
type PhoneBook = [(Name, PhoneNumber)]
inPhoneBook :: Name -> PhoneBook -> Maybe PhoneNumber
inPhoneBook name phoneBook = undefined