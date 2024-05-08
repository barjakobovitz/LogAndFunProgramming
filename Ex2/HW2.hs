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
catMaybes [] = []
catMaybes (x : xs) = case x of
  Nothing -> catMaybes xs
  Just a -> a : catMaybes xs

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
catEithers (x : xs) = case x of
  Left e -> Left e
  Right a -> case catEithers xs of
    Left e -> Left e
    Right as -> Right (a : as)

mapEither :: (a -> Either e b) -> [a] -> Either e [b]
mapEither func xs = catEithers (map func xs)

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers =
  foldr
    ( \x (as, bs) -> case x of
        Left a -> (a : as, bs)
        Right b -> (as, b : bs)
    )
    ([], [])

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right b) = Just b

-- Section 2: Lists
take :: Int -> [a] -> [a]
take n xs = case n of
  0 -> []
  _ -> case xs of
    [] -> []
    (y : ys) -> y : take (n - 1) ys

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x : xs)
  | p x = x : takeWhile p xs
  | otherwise = []

drop :: Int -> [a] -> [a]
drop n xs = case n of
  0 -> xs
  _ -> case xs of
    [] -> []
    (_ : ys) -> drop (n - 1) ys

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile p (x : xs)
  | p x = dropWhile p xs
  | otherwise = x : xs

reverse :: [a] -> [a]
reverse [] = []
reverse (x : xs) = reverse xs ++ [x]

rotate :: Int -> [a] -> [a]
rotate n xs
  | n <= 0 = xs
  | otherwise = rotate (n - 1) (drop (k - 1) xs ++ take (k - 1) xs)
  where
    k = length xs - (n `mod` length xs)

lotate :: Int -> [a] -> [a]
lotate n xs
  | n <= 0 = xs
  | otherwise = lotate (n - 1) (drop 1 xs ++ take 1 xs)

type Generator a = (a -> a, a -> Bool, a)

fromGenerator :: Generator a -> [a]
fromGenerator (f, p, x)
  | p x = f x : fromGenerator (f, p, f x)
  | otherwise = []

replicate :: Int -> a -> [a]
replicate counter val
  | counter > 0 = val : replicate (counter - 1) val
  | otherwise = []

inits :: [a] -> [[a]]
inits [] = [[]]
inits (x : xs) = [] : map (x :) (inits xs)

tails :: [a] -> [[a]]
tails [] = [[]]
tails (x : xs) = (x : xs) : tails xs

-- Section 3: zips and products
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith f (x : xs) (y : ys) = f x y : zipWith f xs ys

zip :: [a] -> [b] -> [(a, b)]
zip [] _ = []
zip _ [] = []
zip (x : xs) (y : ys) = (x, y) : zip xs ys

zipFill :: a -> b -> [a] -> [b] -> [(a, b)]
zipFill _ _ [] [] = []
zipFill defValue1 defValue2 [] (y : ys) = (defValue1, y) : zipFill defValue1 defValue2 [] ys
zipFill defValue1 defValue2 (x : xs) [] = (x, defValue2) : zipFill defValue1 defValue2 xs []
zipFill defValue1 defValue2 (x : xs) (y : ys) = (x, y) : zipFill defValue1 defValue2 xs ys

data ZipFail = ErrorFirst | ErrorSecond deriving (Eq, Show)

zipFail :: [a] -> [b] -> Either ZipFail [(a, b)]
zipFail [] [] = Right []
zipFail [] _ = Left ErrorFirst
zipFail _ [] = Left ErrorSecond
zipFail (x : xs) (y : ys) = case zipFail xs ys of
  Left ErrorFirst -> Left ErrorFirst
  Left ErrorSecond -> Left ErrorSecond
  Right zs -> Right ((x, y) : zs)

unzip :: [(a, b)] -> ([a], [b])
unzip [] = ([], [])
unzip ((x, y) : xs) = case unzip xs of
  (as, bs) -> (x : as, y : bs)

-- Section 4: Knight travels
-- Position (0, 0) is the top-left corner.
data KnightPos = KnightPos {x :: Int, y :: Int} deriving (Show, Eq)

data KnightMove = TopLeft | TopRight | RightTop | RightBottom | BottomRight | BottomLeft | LeftBottom | LeftTop deriving (Enum, Bounded, Show, Eq)

-- Utility to get all knight moves. Don't worry about the implementation of this.
allKnightMoves :: [KnightMove]
allKnightMoves = [minBound .. maxBound]

data Board = Board {width :: Int, height :: Int} deriving (Show, Eq)

tour :: Board -> KnightPos -> Maybe [KnightMove]
tour board start = backtrack [start] []
  where
    backtrack :: [KnightPos] -> [KnightMove] -> Maybe [KnightMove]
    backtrack [] _ = Nothing -- This case ideally shouldn't happen since we always start with at least one element.
    backtrack (currentPos : restVisited) moves
      | length (currentPos : restVisited) == boardSize board = Just (reverse moves) -- If all cells are visited
      | otherwise = foldl' tryMove Nothing (filter (validMove currentPos) allKnightMoves)
      where
        tryMove :: Maybe [KnightMove] -> KnightMove -> Maybe [KnightMove]
        tryMove acc move = case acc of
          Just _ -> acc -- If a valid tour has already been found, skip further processing
          Nothing -> do
            -- Try this move
            let newPos = applyMove currentPos move
            if newPos `elem` (currentPos : restVisited)
              then Nothing
              else backtrack (newPos : currentPos : restVisited) (move : moves)

        -- Checks if a move is valid (stays within board and goes to unvisited position)
        validMove :: KnightPos -> KnightMove -> Bool
        validMove pos move = inBounds board (applyMove pos move) && notElem (applyMove pos move) (pos : restVisited)
        -- Check if the position is within the bounds of the board
        inBounds :: Board -> KnightPos -> Bool
        inBounds (Board w h) (KnightPos x y) = x >= 0 && y >= 0 && x < w && y < h
        -- Calculate the total number of cells on the board
        boardSize :: Board -> Int
        boardSize (Board w h) = w * h

applyMove :: KnightPos -> KnightMove -> KnightPos
applyMove (KnightPos x y) move = case move of
  TopLeft -> KnightPos (x - 2) (y - 1)
  TopRight -> KnightPos (x + 2) (y - 1)
  RightTop -> KnightPos (x + 1) (y - 2)
  RightBottom -> KnightPos (x + 1) (y + 2)
  BottomRight -> KnightPos (x + 2) (y + 1)
  BottomLeft -> KnightPos (x - 2) (y + 1)
  LeftBottom -> KnightPos (x - 1) (y + 2)
  LeftTop -> KnightPos (x - 1) (y - 2)

newtype InvalidPosition = InvalidPosition KnightPos deriving (Show, Eq)

translate :: KnightPos -> [KnightMove] -> [KnightPos]
translate _ [] = []
translate startPos (m : ms) = newPos : translate newPos ms
  where
    newPos = applyMove startPos m

translate' :: [KnightPos] -> Either InvalidPosition [KnightMove]
translate' = checkMoves
  where
    checkMoves :: [KnightPos] -> Either InvalidPosition [KnightMove]
    checkMoves [] = Right []
    checkMoves [_] = Right []
    checkMoves (p1 : p2 : ps) =
      case validKnightMove p1 p2 of
        Just move -> case checkMoves (p2 : ps) of
          Right moves -> Right (move : moves)
          Left err -> Left err
        Nothing -> Left (InvalidPosition p2)

    validKnightMove :: KnightPos -> KnightPos -> Maybe KnightMove
    validKnightMove (KnightPos x1 y1) (KnightPos x2 y2) =
      find (\move -> applyMove (KnightPos x1 y1) move == KnightPos x2 y2) allKnightMoves

-- Bonus (10 points)
mark :: Board -> [KnightPos] -> Either InvalidPosition [[Int]]
mark board positions = do
  let indexedPositions = zip [0 ..] positions
  -- Initialize the board with -1 in all cells
  let initBoard = replicate (height board) (replicate (width board) (-1))
  -- Place the step numbers in their respective positions
  foldl' (placeOnBoard board) (Right initBoard) indexedPositions

placeOnBoard :: Board -> Either InvalidPosition [[Int]] -> (Int, KnightPos) -> Either InvalidPosition [[Int]]
placeOnBoard _ (Left err) _ = Left err
placeOnBoard board (Right brd) (step, KnightPos x y)
  | x < 0 || y < 0 || x >= width board || y >= height board = Left (InvalidPosition (KnightPos x y))
  | brd !! y !! x /= -1 = Left (InvalidPosition (KnightPos x y))
  | otherwise = Right (updateBoard x y step brd)

updateBoard :: Int -> Int -> Int -> [[Int]] -> [[Int]]
updateBoard x y step brd =
  take y brd
    ++ [take x (brd !! y) ++ [step] ++ drop (x + 1) (brd !! y)]
    ++ drop (y + 1) brd
