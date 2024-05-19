{-# LANGUAGE LambdaCase #-}
-- Implement the following functions.
-- When you're done, ghc -Wall -Werror HW2.hs should successfully compile.
-- Tells HLS to show warnings, and the file won't be compiled if there are any warnings, e.g.,
-- eval (-- >>>) won't work.
{-# OPTIONS_GHC -Wall -Werror #-}
-- Refines the above, allowing for unused imports.
{-# OPTIONS_GHC -Wno-unused-imports #-}

module HW3 where

import Data.Either (either, fromLeft, fromRight, isLeft, isRight, lefts, partitionEithers, rights)
import Data.List (find, foldl', isInfixOf, isPrefixOf, isSuffixOf, nub, uncons)
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe, maybe)
import Data.Ratio (denominator, numerator, (%))
import Text.Read (readMaybe)
import Prelude (Bool (..), Char, Either (..), Enum (..), Eq (..), Int, Integer, Maybe (..), Num (..), Ord (..), Rational, Show (..), String, all, and, any, concat, concatMap, const, curry, div, drop, dropWhile, elem, error, even, filter, flip, foldr, fst, id, init, last, length, lines, lookup, map, maximum, minimum, mod, not, notElem, null, odd, or, otherwise, product, reverse, snd, splitAt, sum, tail, take, takeWhile, uncurry, undefined, unlines, unwords, unzip, words, zip, zipWith, (!!), ($), (&&), (++), (.), (||))

-- Section 1: Tree Serialization
data Tree a = Empty | Tree (Tree a) a (Tree a) deriving (Show, Eq)

serialize :: Tree Int -> [Int]
serialize Empty = []
serialize (Tree leftTree x rightTree) = x : serialize leftTree ++ serialize rightTree

deserialize :: [Int] -> Tree Int
deserialize [] = Empty
deserialize (x : xs) = Tree (deserialize left) x (deserialize right)
  where
    left = takeLeft 1 xs
    right = takeRight 1 xs
    takeLeft :: Int -> [Int] -> [Int]
    takeLeft 0 _ = []
    takeLeft n (y : ys) = y : takeLeft (n - 1) ys
    takeLeft _ [] = []
    takeRight :: Int -> [Int] -> [Int]
    takeRight 0 ys = ys
    takeRight n (_ : ys) = takeRight (n - 1) ys
    takeRight _ [] = []


-- Section 2: Infinite lists
data InfiniteList a = a :> InfiniteList a

infixr 5 :>

sample :: InfiniteList a -> [a]
sample = take 10 . itoList

itoList :: InfiniteList a -> [a]
itoList (x :> xs) = x : itoList xs

iiterate :: (a -> a) -> a -> InfiniteList a
iiterate f x = x :> iiterate f (f x)

irepeat :: a -> InfiniteList a
irepeat x = x :> irepeat x

iprepend :: [a] -> InfiniteList a -> InfiniteList a
iprepend [] infList = infList
iprepend (x : xs) infList = x :> iprepend xs infList

itake :: Integer -> InfiniteList a -> [a]
itake n _ | n <= 0 = []
itake n (x :> xs) = x : itake (n - 1) xs

idrop :: Integer -> InfiniteList a -> InfiniteList a
idrop n infList@(_ :> xs) = if n <= 0 then infList else idrop (n - 1) xs

naturals :: InfiniteList Integer
naturals = iiterate (+1) 0

imap :: (a -> b) -> InfiniteList a -> InfiniteList b
imap f (x :> xs) = f x :> imap f xs

ifilter :: (a -> Bool) -> InfiniteList a -> InfiniteList a
ifilter p (x :> xs) = if p x then x :> ifilter p xs else ifilter p xs

ifind :: (a -> Bool) -> InfiniteList a -> a
ifind p (x :> xs) = if p x then x else ifind p xs


iconcat :: InfiniteList [a] -> InfiniteList a
iconcat (x :> xs) = iprepend x (iconcat xs)

integers :: InfiniteList Integer
integers = 0 :> iiterate nextInteger 0
    where
        nextInteger :: Integer -> Integer
        nextInteger x
            | x > 0 = -x
            | otherwise = -x + 1

rationals :: InfiniteList Rational
rationals = interleavePosNeg $ breadthFirstRationals [(1, 1)]
  where
    -- Breadth-first traversal of rationals using a queue
    breadthFirstRationals :: [(Integer, Integer)] -> InfiniteList Rational
    breadthFirstRationals queue = case queue of
        [] -> irepeat 0
        ((a, b) : rest) -> (a % b) :> breadthFirstRationals (rest ++ [(a, a + b), (a + b, b)])

    -- Interleave positive and negative versions of each rational
    interleavePosNeg :: InfiniteList Rational -> InfiniteList Rational
    interleavePosNeg (x :> xs) = x :> negate x :> x :> negate x :> interleavePosNeg xs


-- Bonus: same as rationals, but without repeats!
rationals' :: InfiniteList Rational
rationals' = breadthFirstUniqueRationals [] [(1, 1)]
  where
    -- Breadth-first traversal of unique rationals using a queue
    breadthFirstUniqueRationals :: [Rational] -> [(Integer, Integer)] -> InfiniteList Rational
    breadthFirstUniqueRationals _ [] = irepeat 0
    breadthFirstUniqueRationals seen ((a, b) : rest) =
      let r = a % b
          newSeen = r : (-r) : seen
          newQueue = rest ++ [(a, a + b), (a + b, b)]
      in if r `elem` seen || (-r) `elem` seen
         then breadthFirstUniqueRationals seen newQueue
         else r :> (-r) :> breadthFirstUniqueRationals newSeen newQueue


-- Section 3: Stack Machine
-- Define the data types for stack errors and run errors
data StackError = DivisionByZero | StackUnderflow { instruction :: String, stackValue :: Maybe Int }
    deriving (Show, Eq)

data RunError = InstructionError StackError | ParseError { line :: String }
    deriving (Show, Eq)

-- Define the data type for instructions
data Instruction = Push Int | Pop | Swap | Dup | Add | Sub | Mul | Div
    deriving (Show, Eq)

-- Function to parse a single instruction line
parseInstruction :: String -> Either RunError Instruction
parseInstruction line = 
    case words line of
        ["PUSH", nStr] -> case readMaybe nStr of
            Just n  -> Right (Push n)
            Nothing -> Left (ParseError line)
        ["POP"]       -> Right Pop
        ["SWAP"]      -> Right Swap
        ["DUP"]       -> Right Dup
        ["ADD"]       -> Right Add
        ["SUB"]       -> Right Sub
        ["MUL"]       -> Right Mul
        ["DIV"]       -> Right Div
        _             -> Left (ParseError line)

-- Function to run a single instruction on the stack
runInstruction :: [Int] -> Instruction -> Either StackError [Int]
runInstruction stack (Push n) = Right (n : stack)
runInstruction [] Pop = Left (StackUnderflow "POP" Nothing)
runInstruction (_:xs) Pop = Right xs
runInstruction [] Swap = Left (StackUnderflow "SWAP" Nothing)
runInstruction [_] Swap = Left (StackUnderflow "SWAP" Nothing)
runInstruction (x:y:xs) Swap = Right (y:x:xs)
runInstruction [] Dup = Left (StackUnderflow "DUP" Nothing)
runInstruction (x:xs) Dup = Right (x:x:xs)
runInstruction (x:y:xs) Add = Right ((x + y):xs)
runInstruction [] Add = Left (StackUnderflow "ADD" Nothing)
runInstruction [_] Add = Left (StackUnderflow "ADD" Nothing)
runInstruction (x:y:xs) Sub = Right ((x - y):xs)
runInstruction [] Sub = Left (StackUnderflow "SUB" Nothing)
runInstruction [_] Sub = Left (StackUnderflow "SUB" Nothing)
runInstruction (x:y:xs) Mul = Right ((x * y):xs)
runInstruction [] Mul = Left (StackUnderflow "MUL" Nothing)
runInstruction [_] Mul = Left (StackUnderflow "MUL" Nothing)
runInstruction (x:y:xs) Div
    | y == 0 = Left DivisionByZero
    | otherwise = Right (safeDiv y x:xs)
  where
    safeDiv _ 0 = 0 -- Define the result of division by zero as 0
    safeDiv a b = a `div` b
runInstruction [] Div = Left (StackUnderflow "DIV" Nothing)
runInstruction [_] Div = Left (StackUnderflow "DIV" Nothing)

-- Function to parse and run all instructions from a multi-line string
parseAndRun :: String -> Either RunError [Int]
parseAndRun input = foldr execute (Right []) (reverse (lines input))
  where
    execute line acc = case acc of
        Left err -> Left err
        Right stack -> case parseInstruction line of
            Left err -> Left err
            Right instr -> case runInstruction stack instr of
                Left err -> Left (InstructionError err)
                Right newStack -> Right newStack
