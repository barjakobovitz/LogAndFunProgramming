-- Implement the following functions.
-- When you're done, ghc -Wall -Werror HW0.hs should successfully compile.
--
-- Tells HLS to show warnings, and the file won't be compiled if there are any warnings, e.g.,
-- eval (-- >>>) won't work.
{-# OPTIONS_GHC -Wall -Werror #-}
-- Refines the above, allowing for unused imports.
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- CHANGE MODULE NAME TO HW1 BEFORE SUBMISSION !!! --
module Main where

-- REMOVE IO AND PRINT BEFORE SUBMISSION !!! --
-- These import statement ensures you aren't using any "advanced" functions and types, e.g., lists.
import Prelude (Bool (..), Eq (..), IO, Int, Integer, Num (..), Ord (..), div, error, even, flip, id, mod, not, otherwise, print, undefined, ($), (&&), (.), (||))

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
impossible = undefined -- TODO: ASK DANIEL ABOUT THIS

-- ********* --

-- Section 2

-- ********* --

power :: Integer -> Integer -> Integer
power _ 0 = 1
power base exponent
  | exponent < 0 = error "Exponent must be non-negative"
  | exponent == 1 = base
  | otherwise = base * power base (exponent - 1)

countDigits :: Integer -> Integer
countDigits 0 = 1
countDigits n
  | n < 0 = digitCounter (-n)
  | otherwise = digitCounter n
  where
    digitCounter :: Integer -> Integer
    digitCounter x = count x 0
    count 0 counter = counter
    count x counter = count (x `div` 10) (counter + 1)

toBinary :: Integer -> Integer
toBinary 0 = 0
toBinary 1 = 1
toBinary n
  | n < 0 = -binary (-n)
  | otherwise = binary n
  where
    binary :: Integer -> Integer
    binary x = toBin x 0
    toBin :: Integer -> Integer -> Integer
    toBin 0 _ = 0
    toBin x counter = (x `mod` 2) * power 10 counter + toBin (x `div` 2) (counter + 1)

fromBinary :: Integer -> Integer
fromBinary 0 = 0
fromBinary 1 = 1
fromBinary n
  | n < 0 = -decimal (-n)
  | otherwise = decimal n
  where
    decimal :: Integer -> Integer
    decimal x = toDecimal x 0
    toDecimal :: Integer -> Integer -> Integer
    toDecimal 0 _ = 0
    toDecimal x counter = (x `mod` 10) * power 2 counter + toDecimal (x `div` 10) (counter + 1)

isAbundant :: Integer -> Bool
isAbundant n
  | n <= 0 = False -- Abundance doesn't apply to non-positive numbers
  | otherwise = sumDivs n (n - 1) > n -- Check if the sum of divisors is greater than the number
  where
    sumDivs :: Integer -> Integer -> Integer
    sumDivs x current
      | current < 1 = 0 -- Base case: no divisors below 1
      | x `mod` current == 0 = current + sumDivs x (current - 1) -- Add current if it's a divisor
      | otherwise = sumDivs x (current - 1) -- Continue with the next lower number

rotateDigits :: Integer -> Integer
rotateDigits x
  | x < 0 = -rotateDigits (-x)
  | x < 10 = x
  | otherwise = (x `mod` 10) * power 10 (countDigits (x `div` 10)) + (x `div` 10)

-- ********* --

-- Section 3

-- ********* --

type Generator a = (a -> a, a -> Bool, a)

nullGen :: Generator a -> Bool
nullGen (_, p, x) = not (p x)

-- lastGen :: Generator a -> a
-- lengthGen :: Generator a -> Int
sumGen :: Generator Integer -> Integer
sumGen (f, p, x)
  | nullGen (f, p, x) = 0
  | otherwise = f x + sumGen (f, p, f x)

type Predicate a = a -> Bool

-- anyGen :: Predicate a -> Generator a -> Bool
-- allGen :: Predicate a -> Generator a -> Bool
-- noneGen :: Predicate a -> Generator a -> Bool
-- countGen :: Predicate a -> Generator a -> Int

-- ********* --

-- Section 4

-- ********* --

-- isPrime :: Integer -> Bool
-- isSemiprime :: Integer -> Bool
-- goldbachPair :: Integer -> (Integer, Integer)
-- goldbachPair' :: Integer -> (Integer, Integer)

-- ***** --

-- Bonus

-- ***** --

isCircularPrime :: Integer -> Bool
-- If you choose the implement this function, replace this with the actual implementation
isCircularPrime = undefined

-- ********* REMOVE BEFORE SUBMISSION ********* --

main :: IO ()
main = do
  print "HW1"
  print "Done"