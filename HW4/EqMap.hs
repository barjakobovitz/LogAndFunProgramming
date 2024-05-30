{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -Werror #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module EqMap
  ( EqMap,
    CombiningMap (..),
    empty,
    EqMap.insert, -- To avoid name clash with Data.List.insert
    member,
    remove,
    EqMap.lookup, -- To avoid name clash with Prelude.lookup
    assocs,
    EqMap.fromList
  )
where

import Data.Either
import Data.List
import Data.Maybe
import Data.Semigroup (Arg (..))
import EqSet (EqSet)
import EqSet qualified

data EqMap k v = EqMap (EqSet (Arg k v))

empty :: EqMap k v
empty = EqMap EqSet.empty

member :: (Eq k) => k -> EqMap k v -> Bool
member k (EqMap s) = any (\(Arg k' _) -> k == k') (EqSet.elems s)

insert :: (Eq k) => k -> v -> EqMap k v -> EqMap k v
insert key value (EqMap s) =
  let s' = EqSet.remove (Arg key undefined) s -- Remove any existing entry with the same key
   in EqMap (EqSet.insert (Arg key value) s') -- Insert the new key-value pair

remove :: (Eq k) => k -> EqMap k v -> EqMap k v
remove k (EqMap s) = EqMap (EqSet.remove (Arg k undefined) s)

lookup :: (Eq k) => k -> EqMap k v -> Maybe v
lookup key (EqMap set) = fmap (\(Arg _ v) -> v) . find (\(Arg k _) -> k == key) $ EqSet.elems set

assocs :: EqMap k v -> [(k, v)]
assocs (EqMap s) = map (\(Arg k v) -> (k, v)) (EqSet.elems s)

fromList :: (Eq k) => [(k, v)] -> EqMap k v
fromList = foldr (uncurry EqMap.insert) empty

instance (Eq k, Eq v) => Eq (EqMap k v) where
  EqMap set1 == EqMap set2 = set1 == set2

instance (Show k, Show v) => Show (EqMap k v) where
  show (EqMap s) = "{" ++ intercalate ", " (map (\(Arg k v) -> show k ++ "-> " ++ show v) (EqSet.elems s)) ++ "}"

instance (Eq k) => Semigroup (EqMap k v) where
  EqMap s1 <> EqMap s2 = EqMap (s1 <> s2)

instance (Eq k) => Monoid (EqMap k v) where
  mempty = empty
  mappend = (<>)

newtype CombiningMap k v = CombiningMap {getCombiningMap :: EqMap k v}

instance (Eq k, Semigroup v) => Semigroup (CombiningMap k v) where
  CombiningMap map1 <> CombiningMap map2 = CombiningMap $ foldr combine map1 (assocs map2)
    where
      combine (k, v) acc =
        EqMap.insert
          k
          ( case EqMap.lookup k acc of
              Just vExisting -> vExisting <> v
              Nothing -> v
          )
          acc

instance (Eq k, Semigroup v) => Monoid (CombiningMap k v) where
  mempty = CombiningMap empty
  mappend = (<>)
