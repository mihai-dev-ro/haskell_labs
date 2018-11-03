module Pics where  -- (2p)

import Data.List (intercalate, transpose)
-- import Basics

{-  
    The 'Pic' type designates images.
    All image manipulating functions are to be implemented using
    POINT-FREE STYLE.
    If this is unfamiliar to you, please request assistance :).
    
    Adapted from 'S. Thompson (2011). «Haskell: The Craft of Functional
    Programming» (3rd edition). Addison Wesley'.
-}

{-
    Copied from Basics.
    TO ASK: how to import own modules 
-}

reverseHO2 :: [a] -> [a]
reverseHO2 = foldl (\acc x -> x : acc) []

type Pic = [String] 

cross :: Pic
cross = [ ".#..."
        , "#####"
        , ".#..."
        , ".#..."
        , ".#..."
        ]

{-
    Use 'printPic <somePic>' to nicely display the pic at the console.
-}

printPic :: Pic -> IO ()
printPic = putStrLn . intercalate "\n"

{-
    1. (0.3p)
    Flip along the horizontal axis.
-}

flipH :: Pic -> Pic
flipH = reverseHO2

{-
    2. (0.3p)
    Flip along the vertical axis.
-}

flipV :: Pic -> Pic
flipV = map reverseHO2 

{-
    3. (0.3p)
    Rotate 180 degrees.
-}

rotate :: Pic -> Pic
rotate = reverseHO2 . map reverseHO2

{-
    4. (0.3p)
    Place the first pic on top of the second.
-}

above :: Pic -> Pic -> Pic
above = (++) 

{-
    5. (0.3p)
    Place the first pic to the left of the second.
-}

beside :: Pic -> Pic -> Pic
beside = zipWith (++) 

{-
    6. (0.5p)
    Invert 'colors'.
-}

invert :: Pic -> Pic
invert = 
    let invertCell '.' = '#'
        invertCell '#' = '.'
    in map $ map invertCell 