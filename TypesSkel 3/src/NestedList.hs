module NestedList where

import Data.List
import Classes
import List ()

{-
    Define the 'NestedList' type, for handling lists with an arbitrary
    level of intricateness, which would be impossible to specify
    using plain Haskell lists. For instance, the String representation
    of such a nested list might be "[1, [2, 3], 4, 5]".
    
    Instantiate the following classes with the nested list type:
    * 'Show'
    * 'Functor'
    * 'Container'
    * 'Invertible'
    
    The inversion should be performed DEEPLY i.e., for the elements as well.
-}

data NestedList a = Item a | List [NestedList a]

instance Show a => Show (NestedList a) where
    show (Item x) = show x
    show (List x) = show x

instance Functor NestedList where 
    fmap f (Item x) = Item $ f x 
    fmap f (List x) = List $ map (fmap f) x

instance Container NestedList where
    contents (Item x) = [x]
    contents (List x) = concat $ map contents x

instance (Invertible a) => Invertible (NestedList a) where
    invert (Item x) = Item $ invert x
    invert (List x) = List $ invert x   



