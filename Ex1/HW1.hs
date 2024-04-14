-- Implement the following functions.
-- When you're done, ghc -Wall -Werror HW0.hs should successfully compile.
--
-- Tells HLS to show warnings, and the file won't be compiled if there are any warnings, e.g.,
-- eval (-- >>>) won't work.
{-# OPTIONS_GHC -Wall -Werror #-}
-- Refines the above, allowing for unused imports.
{-# OPTIONS_GHC -Wno-unused-imports #-}

--module HW1 where
module Main where

-- These import statement ensures you aren't using any "advanced" functions and types, e.g., lists.
import Prelude (Bool (..), Eq (..), Int, Integer, Num (..), Ord (..), div, error, even, flip, id, mod, not, otherwise, undefined, ($), (&&), (.), (||) ,IO, putStrLn, print, show, (++))

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
rotate f x y z = f y z x

lotate :: (a -> b -> c -> d) -> b -> c -> a -> d
lotate f x y z = f z x y

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
countDigits 0 = 1
countDigits x
  | x < 0 = countDigits (-x)  -- Handle negative numbers by converting to positive
  | otherwise = countDigitsLoop x  -- Handle all other cases
  where
    countDigitsLoop n
      | n < 10 = 1  -- If the number is less than 10, it's the last digit
      | otherwise = 1 + countDigitsLoop (n `div` 10)  -- Recursively count digits

--toBinary :: Integer -> Integer
power :: Integer -> Integer -> Integer
power _ 0 = 1
power base exponent
    | exponent < 0 = error "Negative exponent not supported"
    | otherwise = base * power base (exponent - 1)

--fromBinary :: Integer -> Integer



-- Recursive helper function to sum divisors
sumDivs :: Integer -> Integer -> Integer
sumDivs n current
  | current == 0    = 0  -- Base case: no divisors below 1
  | n `mod` current == 0 = current + sumDivs n (current - 1)  -- Add current if it's a divisor
  | otherwise       = sumDivs n (current - 1)  -- Continue with the next lower number

-- Function to check if a number is abundant
isAbundant :: Integer -> Bool
isAbundant n
  | n <= 0    = False  -- Abundance doesn't apply to non-positive numbers
  | otherwise = sumDivs n n-1 > n  -- Check if the sum of divisors is greater than the number




rotateDigits :: Integer -> Integer
rotateDigits x
    | x < 0  = -rotateDigits (-x)
    | x < 10 = x
    | otherwise = (x `mod` 10) * power 10 (countDigits (x `div` 10)) + (x `div` 10)
-- ********* --

-- Section 3

-- ********* --

type Generator a = (a -> a, a -> Bool, a)

--nullGen :: Generator a -> Bool
--lastGen :: Generator a -> a
--lengthGen :: Generator a -> Int
--sumGen :: Generator Integer -> Integer

type Predicate a = a -> Bool

--anyGen :: Predicate a -> Generator a -> Bool
--allGen :: Predicate a -> Generator a -> Bool
--noneGen :: Predicate a -> Generator a -> Bool
--countGen :: Predicate a -> Generator a -> Int

-- ********* --

-- Section 4

-- ********* --

--isPrime :: Integer -> Bool
--isSemiprime :: Integer -> Bool
--goldbachPair :: Integer -> (Integer, Integer)
--goldbachPair' :: Integer -> (Integer, Integer)

-- ***** --

-- Bonus

-- ***** --

isCircularPrime :: Integer -> Bool
-- If you choose the implement this function, replace this with the actual implementation
isCircularPrime = undefined

main :: IO ()
main = do
    putStrLn ("isAbundant 9: " ++ show (isAbundant 2))
    putStrLn ("isAbundant -12345: " ++ show (isAbundant (-12345)))
    putStrLn ("isAbundant 12: " ++ show (isAbundant 12))
    putStrLn ("isAbundant 945: " ++ show (isAbundant 945))
