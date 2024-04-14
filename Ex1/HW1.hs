-- Implement the following functions.
-- When you're done, ghc -Wall -Werror HW0.hs should successfully compile.
--
-- Tells HLS to show warnings, and the file won't be compiled if there are any warnings, e.g.,
-- eval (-- >>>) won't work.
{-# OPTIONS_GHC -Wall -Werror #-}
-- Refines the above, allowing for unused imports.
{-# OPTIONS_GHC -Wno-unused-imports #-}

module HW1 where

-- These import statement ensures you aren't using any "advanced" functions and types, e.g., lists.
import Prelude (Bool (..), Eq (..), Int, Integer, Num (..), Ord (..), div, error, even, flip, id, mod, not, otherwise, undefined, ($), (&&), (.), (||))

------------------------------------------------
-- DO NOT MODIFY ANYTHING ABOVE THIS LINE !!! --
------------------------------------------------

-- ********* --

-- Section 1

-- ********* --

const :: a -> b -> a
const a _ = a

(.>) :: (a -> b) -> (b -> c) -> a -> c
f .> g = \x -> g (f x)

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f x y z = f (x, y, z)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

rotate :: (a -> b -> c -> d) -> c -> a -> b -> d
rotate f x y z = f z x y

lotate :: (a -> b -> c -> d) -> b -> c -> a -> d
lotate f x y z = f y z x

-- Generalizations of (.)
(.:) :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
(.:) g f x y z = g (f x y z)

(.:.) :: (e -> f) -> (a -> b -> c -> d -> e) -> a -> b -> c -> d -> f
(.:.) g f x y z w = g (f x y z w)

(.::) :: (f -> g) -> (a -> b -> c -> d -> e -> f) -> a -> b -> c -> d -> e -> g
(.::) g f x y z w v = g (f x y z w v)

(.::.) :: (g -> h) -> (a -> b -> c -> d -> e -> f -> g) -> a -> b -> c -> d -> e -> f -> h
(.::.) g f x y z w v u = g (f x y z w v u)

-- How can we ever implement such a function!?
impossible :: a -> b
impossible = error "impossible" -- TODO: ASK DANIEL ABOUT THIS

-- ********* --

-- Section 2

-- ********* --

countDigits :: Integer -> Integer
toBinary :: Integer -> Integer
fromBinary :: Integer -> Integer
isAbundant :: Integer -> Bool
rotateDigits :: Integer -> Integer

-- ********* --

-- Section 3

-- ********* --

type Generator a = (a -> a, a -> Bool, a)

nullGen :: Generator a -> Bool
lastGen :: Generator a -> a
lengthGen :: Generator a -> Int
sumGen :: Generator Integer -> Integer

type Predicate a = a -> Bool

anyGen :: Predicate a -> Generator a -> Bool
allGen :: Predicate a -> Generator a -> Bool
noneGen :: Predicate a -> Generator a -> Bool
countGen :: Predicate a -> Generator a -> Int

-- ********* --

-- Section 4

-- ********* --

isPrime :: Integer -> Bool
isSemiprime :: Integer -> Bool
goldbachPair :: Integer -> (Integer, Integer)
goldbachPair' :: Integer -> (Integer, Integer)

-- ***** --

-- Bonus

-- ***** --

isCircularPrime :: Integer -> Bool
-- If you choose the implement this function, replace this with the actual implementation
isCircularPrime = undefined
