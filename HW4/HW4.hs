{-# LANGUAGE LambdaCase #-}
-- Implement the following functions.
-- When you're done, ghc -Wall -Werror HW4.hs EqSet.hs EqMap.hs should successfully compile.
-- Tells HLS to show warnings, and the file won't be compiled if there are any warnings, e.g.,
-- eval (-- >>>) won't work.
{-# OPTIONS_GHC -Wall -Werror #-}
-- Refines the above, allowing for unused imports.
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}

module HW4 where

import Data.Char (chr, ord)
import Data.Either
import Data.List
import Data.Maybe
import Data.Semigroup (Arg (..))
import EqMap (EqMap)
import EqMap qualified
import EqSet (EqSet)
import EqSet qualified

-- Section 2: Serialization
class Serializable a where
  serialize :: a -> [Int]
  deserialize :: [Int] -> a

instance Serializable Int where
  serialize :: Int -> [Int]
  serialize x = [x]
  deserialize :: [Int] -> Int
  deserialize [x] = x
  deserialize _ = error "Invalid input"

instance Serializable Bool where
  serialize :: Bool -> [Int]
  serialize True = [1]
  serialize False = [0]
  deserialize :: [Int] -> Bool
  deserialize [0] = False
  deserialize [1] = True
  deserialize _ = error "Invalid input"

instance Serializable Char where
  serialize :: Char -> [Int]
  serialize c = [ord c]
  deserialize :: [Int] -> Char
  deserialize [i] = chr i
  deserialize _ = error "Invalid input"

instance Serializable a => Serializable (Maybe a) where
  serialize :: Serializable a => Maybe a -> [Int]
  serialize Nothing = [0]
  serialize (Just x) = 1 : serialize x
  deserialize :: Serializable a => [Int] -> Maybe a
  deserialize (0 : _) = Nothing
  deserialize (1 : xs) = Just (deserialize xs)
  deserialize _ = error "Invalid input"

instance (Serializable a, Serializable b) => Serializable (a, b) where
  serialize :: (Serializable a, Serializable b) => (a, b) -> [Int]
  serialize (a, b) = serialize a ++ serialize b
  deserialize :: (Serializable a, Serializable b) => [Int] -> (a, b)
  deserialize xs = let (a, b) = splitAt (length xs `div` 2) xs
    in (deserialize a, deserialize b)

instance (Serializable a, Serializable b) => Serializable (Either a b) where
  serialize :: (Serializable a, Serializable b) => Either a b -> [Int]
  serialize (Left x) = 0 : serialize x
  serialize (Right x) = 1 : serialize x
  deserialize :: (Serializable a, Serializable b) => [Int] -> Either a b
  deserialize (0 : xs) = Left (deserialize xs)
  deserialize (1 : xs) = Right (deserialize xs)
  deserialize _ = error "Invalid input"

instance Serializable a => Serializable [a] where
  serialize :: Serializable a => [a] -> [Int]
  serialize xs = length xs : concatMap (\x -> length (serialize x) : serialize x) xs
  deserialize :: Serializable a => [Int] -> [a]
  deserialize [] = []
  deserialize (n:xs) = deserializeList n xs
    where
      deserializeList 0 _ = []
      deserializeList n' (len:rest) =
          let (element, rest') = splitAt len rest
          in deserialize element : deserializeList (n' - 1) rest'
      deserializeList _ _ = error "Invalid input"

instance (Serializable a, Eq a) => Serializable (EqSet a) where
  serialize :: (Serializable a, Eq a) => EqSet a -> [Int]
  serialize = serialize . EqSet.elems
  deserialize :: (Serializable a, Eq a) => [Int] -> EqSet a
  deserialize = EqSet.fromList . deserialize

instance (Serializable k, Eq k, Serializable v) => Serializable (EqMap k v) where
  serialize :: (Serializable k, Eq k, Serializable v) => EqMap k v -> [Int]
  serialize eqMap =
    let pairs = EqMap.assocs eqMap
    in length pairs : concatMap (\(k, v) -> length (serialize k) : serialize k ++ length (serialize v) : serialize v) pairs
  deserialize :: (Serializable k, Eq k, Serializable v) => [Int] -> EqMap k v
  deserialize [] = EqMap.empty
  deserialize (n:xs) = EqMap.fromList (deserializePairs n xs)
    where
      deserializePairs 0 _ = []
      deserializePairs n' (lenK:ks) =
        let (key, rest1) = splitAt lenK ks
        in case rest1 of
            (lenV:rest2) ->
              let (value, rest3) = splitAt lenV rest2
              in (deserialize key, deserialize value) : deserializePairs (n' - 1) rest3
            _ -> error "Invalid input: expected value length"
      deserializePairs _ _ = error "Invalid input"


-- Section 3: Metric
infinity :: Double
infinity = 1 / 0

class Eq a => Metric a where
  distance :: a -> a -> Double

instance Metric Double where
  distance :: Double -> Double -> Double
  distance a b = abs (a - b)

instance Metric Int where
  distance :: Int -> Int -> Double
  distance a b = fromIntegral (abs (a - b))

instance Metric Char where
  distance :: Char -> Char -> Double
  distance a b = fromIntegral (abs (ord a - ord b))

-- Euclidean distance
instance (Metric a, Metric b) => Metric (a, b) where
  distance :: (Metric a, Metric b) => (a, b) -> (a, b) -> Double
  distance (a1, b1) (a2, b2) = sqrt (distance a1 a2 ** 2 + distance b1 b2 ** 2)

data ManhattanTuple a b = ManhattanTuple a b deriving Eq
instance (Metric a, Metric b) => Metric (ManhattanTuple a b) where
  distance :: (Metric a, Metric b) => ManhattanTuple a b -> ManhattanTuple a b -> Double
  distance (ManhattanTuple a1 b1) (ManhattanTuple a2 b2) = distance a1 a2 + distance b1 b2

-- Just and Nothing have distance of infinity.
-- Two Justs measure the distance between the two values.
instance Metric a => Metric (Maybe a) where
  distance :: Metric a => Maybe a -> Maybe a -> Double
  distance Nothing Nothing = 0
  distance (Just a) (Just b) = distance a b
  distance _ _ = infinity

-- Left and Right have a distance of infinity.
-- Same constructores measure the distance between the two values.
instance (Metric a, Metric b) => Metric (Either a b) where
  distance :: (Metric a, Metric b) => Either a b -> Either a b -> Double
  distance (Left a1) (Left a2) = distance a1 a2
  distance (Right b1) (Right b2) = distance b1 b2
  distance _ _ = infinity

-- Lists of different sizes have distance of infinity.
-- Euclidean distance.
instance Metric a => Metric [a] where
  distance [] [] = 0
  distance [] _ = infinity
  distance _ [] = infinity
  distance xs ys
    | length xs /= length ys = infinity
    | otherwise = sqrt . sum $ zipWith (\x y -> distance x y ** 2) xs ys



newtype ManhattanList a = ManhattanList [a] deriving Eq
instance Metric a => Metric (ManhattanList a) where
  distance :: Metric a => ManhattanList a -> ManhattanList a -> Double
  distance (ManhattanList xs) (ManhattanList ys) = sum (zipWith distance xs ys)

-- Returns the element with the shortest distance to the input.
-- If there are no numbers whose distance is less than infinity, return Nothing.
closest :: Metric a => a -> [a] -> Maybe a
closest x = foldr (\y acc -> case acc of
  Nothing -> if distance x y < infinity then Just y else Nothing
  Just z -> if distance x y < distance x z then Just y else acc) Nothing
-- Similar to the above, but uses a function move the element
-- to another metric space.
closestOn :: Metric b => (a -> b) -> a -> [a] -> Maybe a
closestOn f x = foldr (\y acc -> case acc of
  Nothing -> Just y
  Just z -> if distance (f x) (f y) < distance (f x) (f z) then Just y else acc) Nothing
-- Will not swap elements whose distance is less than d, even if their
-- order implies they should be swapped.
metricBubbleSort :: (Ord a, Metric a) => Double -> [a] -> [a]
metricBubbleSort d xs = if sorted xs then xs else metricBubbleSort d (bubble xs)
  where
    bubble [] = []
    bubble [x] = [x]
    bubble (x:y:zs)
      | x > y && distance x y >= d = y : bubble (x : zs)
      | otherwise = x : bubble (y : zs)
    sorted [] = True
    sorted [_] = True
    sorted (x:y:zs) = (x <= y || distance x y < d) && sorted (y:zs)
-- Similar to the above, but uses a function to extract the value used for sorting.
metricBubbleSortOn :: (Metric b, Ord b) => (a -> b) -> Double -> [a] -> [a]
metricBubbleSortOn f d = repeatSort
  where
    bubble [] = (False, [])
    bubble [x] = (False, [x])
    bubble (x:y:zs)
      | f x > f y && distance (f x) (f y) >= d = let (_, rest) = bubble (x : zs)
                                                  in (True, y : rest)
      | otherwise = let (swapped, rest) = bubble (y : zs)
                     in (swapped, x : rest)
    -- Repeat the process until no more swaps are needed
    repeatSort lst = let (swapped, sorted) = bubble lst
                      in if swapped then repeatSort sorted else sorted



--bonus
-- Define the depth-first search function for regular values
dfs :: (Metric a, Eq a) => [a] -> a -> [a]
dfs xs start = dfs' [start] []
  where
    dfs' [] visited = reverse visited
    dfs' (x:stack) visited
      | x `elem` visited = dfs' stack visited
      | otherwise =
          let neighbors = filter (\y -> distance x y < infinity && y `notElem` visited) xs
          in dfs' (neighbors ++ stack) (x : visited)

-- Define the depth-first search function for Maybe values
dfsMaybe :: (Metric a, Eq a) => [Maybe a] -> Maybe a -> [Maybe a]
dfsMaybe xs start = dfs' [start] []
  where
    dfs' [] visited = reverse visited
    dfs' (Nothing : stack) visited =
      let nothings = filter isNothing xs
          remainingNothings = nothings \\ visited
      in if null remainingNothings
          then dfs' stack visited
          else dfs' (stack ++ remainingNothings) (visited ++ nothings)
    dfs' (Just x:stack) visited
      | Just x `elem` visited = dfs' stack visited
      | otherwise =
          let neighbors = filter (\y -> case y of
                                          Nothing -> False
                                          Just y' -> distance x y' < infinity) xs
          in dfs' (stack ++ neighbors) (Just x : visited)

-- Clustering function for regular values
clusters' :: (Metric a, Eq a) => [a] -> [[a]]
clusters' [] = []
clusters' (x:xs) =
  let clusterRegular = dfs (x:xs) x
      remaining = filter (`notElem` clusterRegular) xs
  in clusterRegular : clusters' remaining

-- Clustering function for Maybe values
clustersMaybe :: (Metric a, Eq a) => [Maybe a] -> [[Maybe a]]
clustersMaybe [] = []
clustersMaybe (x:xs) =
  let clusterMaybe = dfsMaybe (x:xs) x
      remaining = filter (`notElem` clusterMaybe) xs
  in clusterMaybe : clustersMaybe remaining

-- Type class for clustering strategy
class Clusterable a where
  cluster :: (Metric b, Eq b) => [a b] -> [[a b]]

instance Clusterable [] where
  cluster :: (Metric b, Eq b) => [b] -> [[b]]
  cluster = clusters'

instance Clusterable Maybe where
  cluster :: (Metric b, Eq b) => [Maybe b] -> [[Maybe b]]
  cluster = clustersMaybe

-- General clustering function
clusters :: (Clusterable f, Metric a, Eq a) => [f a] -> [[f a]]
clusters = cluster