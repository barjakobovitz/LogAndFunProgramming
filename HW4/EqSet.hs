{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -Werror #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module EqSet
  ( EqSet,
    empty,
    EqSet.insert, -- To avoid name clash with Data.List.insert
    member,
    remove,
    elems,
    EqSet.fromList
  )
where

import Data.Either
import Data.List
import Data.Maybe

newtype EqSet a = EqSet {getSet :: [a]}

empty :: EqSet a
empty = EqSet []

member :: (Eq a) => a -> EqSet a -> Bool
member x (EqSet xs) = elem x xs

insert :: (Eq a) => a -> EqSet a -> EqSet a
insert x s@(EqSet xs) = if x `member` s then s else EqSet (x : xs)

remove :: (Eq a) => a -> EqSet a -> EqSet a
remove x (EqSet xs) = EqSet (delete x xs)

elems :: EqSet a -> [a]
elems (EqSet xs) = xs

fromList :: (Eq a) => [a] -> EqSet a
fromList = foldr EqSet.insert empty

instance (Eq a) => Eq (EqSet a) where
  EqSet xs == EqSet ys = all (`elem` ys) xs && all (`elem` xs) ys

instance (Show a) => Show (EqSet a) where
  show eqSet = "{" ++ intercalate ", " (map show (getSet eqSet)) ++ "}"

instance (Eq a) => Semigroup (EqSet a) where
  (EqSet xs) <> (EqSet ys) = EqSet $ foldr insertIfNotPresent ys xs
    where
      insertIfNotPresent x acc = if x `elem` acc then acc else x : acc

instance (Eq a) => Monoid (EqSet a) where
  mempty = empty
  mappend = (<>)
