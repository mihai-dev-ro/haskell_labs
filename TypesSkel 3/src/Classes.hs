module Classes where

import Data.Char
import Data.Bool

{-
    A class for container types, which are able to enumerate their elements
    using a Haskell list. What is the kind of 'c'?

    Answer: * -> * (a type constructor)
-}
class Container c where
    contents :: c a -> [a]

{-
    A class for types with invertible values. What is the kind of 'a'?
    The default invert operation is the identity.

    Answer: * (= as in concrete type)
-}
class Invertible a where
    invert :: a -> a
    invert = id

{-
    Primitive types are instances of the 'Invert' class.
    According to the default definition of 'invert', nothing is actually
    performed onto the primitive values.
-}
instance Invertible Char where
    invert x 
        | isAlpha x = if isUpper x then toLower x else toUpper x
        | otherwise = x

instance Invertible Bool where
    invert = not

instance Invertible Int where
    invert = negate

instance Invertible Integer where
    invert = negate

instance Invertible Float where 
    invert = negate

instance Invertible Double where
    invert = negate

--instance Num a => Invertible a where
--    invert = negate