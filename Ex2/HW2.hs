{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -Werror #-}
-- Refines the above, allowing for unused imports.
{-# OPTIONS_GHC -Wno-unused-imports #-}

module HW2 where

import Data.List (find, foldl')
import Prelude (Bool (..), Bounded (..), Char, Either (..), Enum (..), Eq (..), Int, Integer, Maybe (..), Num (..), Ord (..), Show (..), String, all, and, any, concat, concatMap, const, curry, div, elem, error, even, filter, flip, foldl, foldr, fst, id, length, lines, lookup, map, mod, not, notElem, null, odd, otherwise, product, snd, sum, uncurry, undefined, unlines, unwords, words, (!!), ($), (&&), (++), (.), (||))

------------------------------------------------
-- DO NOT MODIFY ANYTHING ABOVE THIS LINE !!! --
------------------------------------------------

-- Section 1.1: Basic Maybes
concatMaybeMap :: (a -> Maybe b) -> Maybe a -> Maybe b
concatMaybeMap _ Nothing = Nothing
concatMaybeMap f (Just x) = f x

fromMaybe :: a -> Maybe a -> a
fromMaybe defaultValue Nothing = defaultValue
fromMaybe _ (Just x) = x

maybe :: b -> (a -> b) -> Maybe a -> b
maybe defaultValue _ Nothing = defaultValue
maybe _ f (Just x) = f x

catMaybes :: [Maybe a] -> [a]
catMaybes []=[]
catMaybes (x:xs)= case x of
    Nothing-> catMaybes xs
    Just a-> a : catMaybes xs

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe func xs = catMaybes (map func xs)

-- Section 1.2 Basic Eithers
concatEitherMap :: (a -> Either e b) -> Either e a -> Either e b
concatEitherMap _ (Left e) = Left e
concatEitherMap f (Right x) = f x

either :: (a -> c) -> (b -> c) -> Either a b -> c
either f _ (Left x) = f x
either _ g (Right x) = g x

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left x) = Left (f x)
mapLeft _ (Right x) = Right x

catEithers :: [Either e a] -> Either e [a]
catEithers [] = Right []
catEithers (x:xs) = case x of
    Left e -> Left e
    Right a -> case catEithers xs of
        Left e -> Left e
        Right as -> Right (a:as)

mapEither :: (a -> Either e b) -> [a] -> Either e [b]
mapEither func xs = catEithers (map func xs)

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers = foldr (\x (as, bs) -> case x of
    Left a -> (a:as, bs)
    Right b -> (as, b:bs)) ([], [])


eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right b) = Just b

-- Section 2: Lists
-- take :: Int -> [a] -> [a]
-- takeWhile :: (a -> Bool) -> [a] -> [a]
-- drop :: Int -> [a] -> [a]
-- dropWhile :: (a -> Bool) -> [a] -> [a]
-- reverse :: [a] -> [a]
-- rotate :: Int -> [a] -> [a]
-- lotate :: Int -> [a] -> [a]
-- type Generator a = (a -> a, a -> Bool, a)
-- fromGenerator :: Generator a -> [a]
-- replicate :: Int -> a -> [a]
-- inits :: [a] -> [[a]]
-- tails :: [a] -> [[a]]

-- Section 3: zips and products
-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
-- zip :: [a] -> [b] -> [(a, b)]
-- zipFill :: a -> b -> [a] -> [b] -> [(a, b)]
-- data ZipFail = ErrorFirst | ErrorSecond deriving (Eq, Show)
-- zipFail :: [a] -> [b] -> Either ZipFail [(a, b)]
-- unzip :: [(a, b)] -> ([a], [b])

-- Section 4: Knight travels
-- -- Position (0, 0) is the top-left corner.
-- data KnightPos = KnightPos {x :: Int, y :: Int} deriving (Show, Eq)
-- data KnightMove = TopLeft | TopRight | RightTop | RightBottom | BottomRight | BottomLeft | LeftBottom | LeftTop deriving (Enum, Bounded, Show, Eq)
-- -- Utility to get all knight moves. Don't worry about the implementation of this.
-- allKnightMoves :: [KnightMove]
-- allKnightMoves = [minBound .. maxBound]
-- data Board = Board {width :: Int, height :: Int} deriving (Show, Eq)
-- tour :: Board -> KnightPos -> Maybe [KnightMove]
-- newtype InvalidPosition = InvalidPosition KnightPos deriving (Show, Eq)
-- translate :: KnightPos -> [KnightMove] -> [KnightPos]
-- translate' :: [KnightPos] -> Either InvalidPosition [KnightMove]

-- Bonus (10 points)
-- mark :: Board -> [KnightPos] -> Either InvalidPosition [[Int]]
