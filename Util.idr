module Util

import Data.AVL.Dict


||| Create the reverse mapping for a given dictionary.
|||
||| Elements are appended to a list, in case the mapping isn't 1:1.
|||
||| XXX: generalize to `transposeWith`, uses a function which to
|||      fold elements sharing the same key in the input dict.
||| XXX: move me to a utils library.
||| XXX: Potentially contribute this upstream to the containers library.
export total
transpose : Eq a => Ord a => Eq b => Ord b => Dict a b -> Dict b (List a)
transpose dict = let
  -- XXX: I spent way too much time trying to do this without creating
  -- an intermediate list. How to avoid this?
  swapped = map swap (toList dict)
in
  Foldable.foldl foldIn Dict.empty (map swap (toList dict))
where
  foldIn : Dict b (List a) -> (b, a) -> Dict b (List a)
  foldIn accum (k, v) = case lookup k accum of
    Nothing => Dict.insert k [v]       accum
    Just vs => Dict.insert k (v :: vs) accum


||| XXX: should be library function
export
unwrapLeft : Either a b -> a
unwrapLeft (Left x) = x


||| XXX: should be library function
export
unwrapRight: Either a b -> b
unwrapRight (Right y) = y


||| XXX: should be a library function
export total
fromMaybe : e -> Maybe a -> Either e a
fromMaybe err Nothing         = Left  err
fromMaybe err (Just whatever) = Right whatever


||| XXX: should be a library function
export total
foldDict : Eq a => Ord a => a  -> Nat -> Dict a Nat -> Dict a Nat
foldDict k v accum = case Dict.lookup k accum of
  Nothing => insert k v       accum
  Just x  => insert k (v + x) accum
