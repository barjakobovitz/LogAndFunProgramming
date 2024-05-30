{-# LANGUAGE LambdaCase #-}
-- Implement the following functions.
-- When you're done, ghc -Wall -Werror HW4.hs EqSet.hs EqMap.hs should successfully compile.
-- Tells HLS to show warnings, and the file won't be compiled if there are any warnings, e.g.,
-- eval (-- >>>) won't work.
{-# OPTIONS_GHC -Wall -Werror #-}
-- Refines the above, allowing for unused imports.
{-# OPTIONS_GHC -Wno-unused-imports #-}

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
  serialize x = [x]
  deserialize [x] = x
  deserialize _ = error "Invalid input"

instance Serializable Bool where
  serialize True = [1]
  serialize False = [0]
  deserialize [0] = False
  deserialize [1] = True
  deserialize _ = error "Invalid input"

instance Serializable Char where
  serialize c = [ord c]
  deserialize [i] = chr i
  deserialize _ = error "Invalid input"

instance Serializable a => Serializable (Maybe a) where
  serialize Nothing = [0]
  serialize (Just x) = 1 : serialize x
  deserialize (0 : _) = Nothing
  deserialize (1 : xs) = Just (deserialize xs)
  deserialize _ = error "Invalid input"

instance (Serializable a, Serializable b) => Serializable (a, b) where
  serialize (a, b) = serialize a ++ serialize b
  deserialize xs = let (a, b) = splitAt (length xs `div` 2) xs
    in (deserialize a, deserialize b)

instance (Serializable a, Serializable b) => Serializable (Either a b) where
  serialize (Left x) = 0 : serialize x
  serialize (Right x) = 1 : serialize x
  deserialize (0 : xs) = Left (deserialize xs)
  deserialize (1 : xs) = Right (deserialize xs)
  deserialize _ = error "Invalid input"

instance Serializable a => Serializable [a] where
  serialize xs = length xs : concatMap (\x -> length (serialize x) : serialize x) xs
  deserialize [] = []
  deserialize (n:xs) = deserializeList n xs
    where
      deserializeList 0 _ = []
      deserializeList n' (len:rest) =
          let (element, rest') = splitAt len rest
          in deserialize element : deserializeList (n' - 1) rest'
      deserializeList _ _ = error "Invalid input"
      
instance (Serializable a, Eq a) => Serializable (EqSet a) where
  serialize = serialize . EqSet.elems
  deserialize = EqSet.fromList . deserialize
 
instance (Serializable k, Eq k, Serializable v) => Serializable (EqMap k v) where
  serialize eqMap = 
    let pairs = EqMap.assocs eqMap
    in length pairs : concatMap (\(k, v) -> length (serialize k) : serialize k ++ length (serialize v) : serialize v) pairs
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


-- -- Section 3: Metric
-- infinity :: Double
-- infinity = 1 / 0

-- class Eq a => Metric a where
--   distance :: a -> a -> Double

-- instance Metric Double where
--   distance a b = abs (a - b)
-- instance Metric Int where
--   distance a b = fromIntegral (abs (a - b))
-- instance Metric Char where
--   distance a b = fromIntegral (abs (ord a - ord b))

-- -- Euclidean distance
-- instance (Metric a, Metric b) => Metric (a, b) where
--   distance (a1, b1) (a2, b2) = sqrt (distance a1 a2 ^ 2 + distance b1 b2 ^ 2)  

-- data ManhattanTuple a b = ManhattanTuple a b deriving Eq
-- instance (Metric a, Metric b) => Metric (ManhattanTuple a b) where
--   distance (ManhattanTuple a1 b1) (ManhattanTuple a2 b2) = distance a1 a2 + distance b1 b2

-- -- Just and Nothing have distance of infinity.
-- -- Two Justs measure the distance between the two values.
-- instance Metric a => Metric (Maybe a) where
--   distance Nothing Nothing = 0
--   distance (Just a) (Just b) = distance a b
--   distance _ _ = infinity

-- -- Left and Right have a distance of infinity.
-- -- Same constructores measure the distance between the two values.
-- instance (Metric a, Metric b) => Metric (Either a b) where
--   distance (Left a1) (Left a2) = distance a1 a2
--   distance (Right b1) (Right b2) = distance b1 b2
--   distance _ _ = infinity

-- -- Lists of different sizes have distance of infinity.
-- -- Euclidean distance.
-- instance Metric a => Metric [a] where
--   distance [] [] = 0
--   distance [] _ = infinity
--   distance _ [] = infinity
--   distance (x : xs) (y : ys) = distance x y + distance xs ys

-- newtype ManhattanList a = ManhattanList [a] deriving Eq
-- instance Metric a => Metric (ManhattanList a) where
--   distance (ManhattanList xs) (ManhattanList ys) = sum (zipWith distance xs ys)

-- -- Returns the element with the shortest distance to the input.
-- -- If there are no numbers whose distance is less than infinity, return Nothing.
-- closest :: Metric a => a -> [a] -> Maybe a
-- closest = undefined
-- -- Similar to the above, but uses a function move the element
-- -- to another metric space.
-- closestOn :: Metric b => (a -> b) -> a -> [a] -> Maybe a
-- closestOn = undefined
-- -- Will not swap elements whose distance is less than d, even if their
-- -- order implies they should be swapped.
-- metricBubbleSort :: (Metric a, Ord a) => Double -> [a] -> [a]
-- metricBubbleSort = undefined
-- -- Similar to the above, but uses a function to extract the value used for sorting.
-- metricBubbleSortOn :: (Metric b, Ord b) => (a -> b) -> Double -> [a] -> [a]
-- metricBubbleSortOn = undefined
  

-- -- Bonus (10 points).
-- clusters :: Metric a => [a] -> [[a]]
-- clusters = undefined
