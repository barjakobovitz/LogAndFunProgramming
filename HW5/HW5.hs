{-# LANGUAGE LambdaCase #-}
-- Implement the following functions.
-- When you're done, ghc -Wall -Werror Deque.hs HW5.hs
-- Tells HLS to show warnings, and the file won't be compiled if there are any warnings, e.g.,
-- eval (-- >>>) won't work.
{-# OPTIONS_GHC -Wall -Werror #-}
-- Refines the above, allowing for unused imports.
{-# OPTIONS_GHC -Wno-unused-imports #-}

module HW5 where

import Control.Applicative (liftA2)
import Data.Char (ord)
import Data.Either
import Data.List (foldl', uncons)
import Data.Map (Map, (!?))
import Data.Map qualified as M
import Data.Maybe
import Data.Monoid (All (..), Any (..), First (..), Last (..), Product (..), Sum (..))
import Data.Ord (Down (..))
import Data.Semigroup (Arg (..), Max (..), Min (..))
import Data.Set (Set)
import Data.Set qualified as S
import Deque (Deque)
import Deque qualified as DQ


data FoldMapFunc a m result = FoldMapFunc {agg :: a -> m, finalize :: m -> result}

foldMap' :: (Foldable t, Monoid m) => FoldMapFunc a m result -> t a -> result
foldMap' FoldMapFunc{agg, finalize} = finalize . foldMap agg

-- Section 1: Foldable functions
fmsum :: Num a => FoldMapFunc a (Sum a) a
fmsum = FoldMapFunc Sum getSum
fmor :: FoldMapFunc Bool Any Bool
fmor = FoldMapFunc Any getAny
fmfold :: Monoid a => FoldMapFunc a a a
fmfold = FoldMapFunc id id
fmelem :: Eq a => a -> FoldMapFunc a Any Bool
fmelem x = FoldMapFunc (\y -> Any (x == y)) getAny
fmfind :: (a -> Bool) -> FoldMapFunc a (First a) (Maybe a)
fmfind pred1 = FoldMapFunc (\x -> First (if pred1 x then Just x else Nothing)) getFirst
fmlength :: FoldMapFunc a (Sum Int) Int
fmlength = FoldMapFunc (const (Sum 1)) getSum
fmnull :: FoldMapFunc a All Bool
fmnull = FoldMapFunc (const (All False)) getAll
fmmaximum :: Ord a => FoldMapFunc a (Maybe (Max a)) (Maybe a)
fmmaximum = FoldMapFunc (Just . Max) getMaxMaybe
  where
    getMaxMaybe :: Maybe (Max a) -> Maybe a
    getMaxMaybe = fmap getMax
fmminimum :: Ord a => FoldMapFunc a (Maybe (Min a)) (Maybe a)
fmminimum = FoldMapFunc (Just . Min) getMinMaybe
      where
        getMinMaybe :: Maybe (Min a) -> Maybe a
        getMinMaybe = fmap getMin
fmmaxBy :: Ord b => (a -> b) -> FoldMapFunc a (Maybe (Max (Arg b a))) (Maybe a)
fmmaxBy f = FoldMapFunc (Just . Max . (\x -> Arg (f x) x)) getMaxByMaybe
          where
            getMaxByMaybe :: Maybe (Max (Arg b a)) -> Maybe a
            getMaxByMaybe = fmap (\(Max (Arg _ a)) -> a)
fmminBy :: Ord b => (a -> b) -> FoldMapFunc a (Maybe (Min (Arg b a))) (Maybe a)
fmminBy f = FoldMapFunc (Just . Min . (\x -> Arg (f x) x)) getMinByMaybe
              where
                getMinByMaybe :: Maybe (Min (Arg b a)) -> Maybe a
                getMinByMaybe = fmap (\(Min (Arg _ a)) -> a)
fmtoList :: FoldMapFunc a [a] [a]
fmtoList = FoldMapFunc (: []) id

-- -- Section 2: Deque instances (Don't forget to implement the instances in Deque.hs as well!)
newtype DequeWrapper a = DequeWrapper (Deque a) deriving (Show, Eq)
instance Semigroup (DequeWrapper a) where
  DequeWrapper dq1 <> DequeWrapper dq2 = DequeWrapper (dq1 <> dq2)
instance Monoid (DequeWrapper a) where
  mempty = DequeWrapper DQ.empty
instance Foldable DequeWrapper where
  foldMap f (DequeWrapper dq) = foldMap f dq
instance Functor DequeWrapper where
  fmap f (DequeWrapper dq) = DequeWrapper (fmap f dq)
instance Applicative DequeWrapper where
  pure x = DequeWrapper (pure x)
  DequeWrapper dq1 <*> DequeWrapper dq2 = DequeWrapper (dq1 <*> dq2)
instance Monad DequeWrapper where
  DequeWrapper dq >>= f = DequeWrapper (dq >>= (\x -> case f x of DequeWrapper dq' -> dq')) 

-- Section 3: Calculator and traverse
class Monad f => CalculatorError f where
  divideByZero :: f Int
  missingVariable :: String -> f Int

  runCalculator :: CalculatorError f => Map String Int -> Expr -> f Int
  runCalculator env = \case
      Val n -> pure n
      Var x -> case M.lookup x env of
          Just n -> pure n
          Nothing -> missingVariable x
      Add e1 e2 -> liftA2 (+) (runCalculator env e1) (runCalculator env e2)
      Sub e1 e2 -> liftA2 (-) (runCalculator env e1) (runCalculator env e2)
      Mul e1 e2 -> liftA2 (*) (runCalculator env e1) (runCalculator env e2)
      Div e1 e2 -> do
          n1 <- runCalculator env e1
          n2 <- runCalculator env e2
          if n2 == 0 then
              divideByZero
          else
              pure (n1 `div` n2)
  

-- Instances to implement:
instance CalculatorError Maybe where
    divideByZero = Nothing
    missingVariable _ = Nothing

data Err = DivByZero | MissingVar String deriving (Show, Eq)
instance CalculatorError (Either Err) where
    divideByZero = Left DivByZero
    missingVariable var = Left (MissingVar var)

data Defaults
  = Defaults
  -- This replaces the entire division result, not just the denominator!
  { defaultForDivisionByZero :: Int
  , defaultForVariable :: String -> Int
  }
defaults :: Defaults
defaults =
  Defaults
    { defaultForDivisionByZero = 0
    , defaultForVariable = sum . map ord
    }
instance CalculatorError (Reader Defaults) where
    divideByZero = Reader $ \defaults' -> defaultForDivisionByZero defaults'
    missingVariable var = Reader $ \defaults' -> defaultForVariable defaults' var


-- From the lectures:
newtype Reader r a = Reader {runReader :: r -> a}
instance Functor (Reader r) where
  fmap f r = Reader $ f . runReader r
instance Applicative (Reader r) where
  pure = Reader . const
  liftA2 f ra rb = Reader $ \r -> f (runReader ra r) (runReader rb r)
instance Monad (Reader r) where
  ra >>= f = Reader $ \r -> runReader (f $ runReader ra r) r

data Expr
  = Val Int
  | Var String
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  deriving (Show, Eq)

exprs1 :: [Expr]
exprs1 = [Val 1, Add (Var "x") (Val 2), Div (Val 30) (Var "y"), Mul (Var "x") (Var "y")]
exprs2 :: [Expr]
exprs2 = [Val 1, Add (Var "z") (Val 2), Div (Var "y") (Val 0)]
variables :: Map String Int
variables = M.fromList [("x", 10), ("y", 20)]

go :: CalculatorError f => [Expr] -> f [Int]
go = traverse (runCalculator variables)

hangman :: String -> IO Int
hangman word = do
    attempts <- gameLoop word S.empty 0
    let score = attempts 
    return score

gameLoop :: String -> Set Char -> Int -> IO Int
gameLoop word guessed attempts = do
    putStrLn $ displayWord word guessed
    if all (`S.member` guessed) (filter (/= ' ') (map toLower word))
        then do
            putStrLn "Very good, the word is:"
            putStrLn word
            return attempts
        else do
            putStr "Guess a letter: "
            guess <- getChar
            _ <- getLine -- To consume the newline character
            if guess == '?'
                then do
                    putStrLn $ "Remaining letters: " ++ remainingLetters guessed
                    gameLoop word guessed attempts
                else
                    let lowerGuess = toLower guess
                    in if not (isLetter lowerGuess)
                        then do
                            putStrLn $ "Invalid letter guess " ++ [guess] ++ "!"
                            gameLoop word guessed attempts
                        else if lowerGuess `S.member` guessed
                            then do
                              if lowerGuess `elem` map toLower word
                                then do
                                putStrLn "You've already guessed that letter."
                                gameLoop word guessed attempts
                                else do
                                  putStrLn "Wrong guess!"
                                  gameLoopTryAgain word guessed (attempts + 1)
                            else
                                let newGuessed = S.insert lowerGuess guessed
                                in if lowerGuess `elem` map toLower word
                                    then gameLoop word newGuessed attempts
                                    else do
                                        putStrLn "Wrong guess!"
                                        gameLoopTryAgain word newGuessed (attempts + 1)

gameLoopTryAgain :: String -> Set Char -> Int -> IO Int
gameLoopTryAgain word guessed attempts = do
    putStrLn $ displayWord word guessed
    if all (`S.member` guessed) (filter (/= ' ') (map toLower word))
        then do
            putStrLn "Very good, the word is:"
            putStrLn word
            return attempts
        else do
            putStr "Try again: "
            guess <- getChar
            _ <- getLine -- To consume the newline character
            if guess == '?'
                then do
                    putStrLn $ "Remaining letters: " ++ remainingLetters guessed
                    gameLoopTryAgain word guessed attempts
                else
                    let lowerGuess = toLower guess
                    in if not (isLetter lowerGuess)
                        then do
                            putStrLn $ "Invalid letter guess " ++ [guess] ++ "!"
                            gameLoopTryAgain word guessed attempts
                        else if lowerGuess `S.member` guessed  
                            then do
                              if lowerGuess `elem` map toLower word
                              then do
                                putStrLn "You've already guessed that letter."
                                gameLoopTryAgain word guessed attempts
                              else do
                                putStrLn "Wrong guess!"
                                gameLoopTryAgain word guessed (attempts + 1)
                            else
                                let newGuessed = S.insert lowerGuess guessed
                                in if lowerGuess `elem` map toLower word
                                    then gameLoop word newGuessed attempts
                                    else do
                                        putStrLn "Wrong guess!"
                                        gameLoopTryAgain word newGuessed (attempts + 1)

displayWord :: String -> Set Char -> String
displayWord word guessed = unwords $ map (\c -> if toLower c `S.member` guessed || c == ' ' then [c] else "_") word

isLetter :: Char -> Bool
isLetter c = c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z'

toLower :: Char -> Char
toLower c
    | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
    | otherwise = c

remainingLetters :: Set Char -> String
remainingLetters guessed = "[" ++ [c | c <- ['a'..'z'], not (c `S.member` guessed)] ++ "]"


