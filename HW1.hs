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
(.>) :: (a -> b) -> (b -> c) -> a -> c
curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
rotate :: (a -> b -> c -> d) -> c -> a -> b -> d
lotate :: (a -> b -> c -> d) -> b -> c -> a -> d
-- Generalizations of (.)
(.:) :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
(.:.) :: (e -> f) -> (a -> b -> c -> d -> e) -> a -> b -> c -> d -> f
(.::) :: (f -> g) -> (a -> b -> c -> d -> e -> f) -> a -> b -> c -> d -> e -> g
(.::.) :: (g -> h) -> (a -> b -> c -> d -> e -> f -> g) -> a -> b -> c -> d -> e -> f -> h
-- How can we ever implement such a function!?
impossible :: a -> b

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
