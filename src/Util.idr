module Util

import Data.HashMap


%default total


||| Create the reverse mapping for a given dictionary.
|||
||| Elements are appended to a list, in case the mapping isn't 1:1.
|||
||| XXX: generalize to `transposeWith`, uses a function which to
|||      fold elements sharing the same key in the input dict.
||| XXX: move me to a utils library.
||| XXX: Potentially contribute this upstream to the containers library.
export
transpose : Eq a => Eq b => Hashable a => Hashable b => HashMap a b -> HashMap b (List a)
transpose dict =
  -- XXX: I spent way too much time trying to do this without creating
  -- an intermediate list. How to avoid this?
  let swapped = map swap (HashMap.toList dict)
  in foldl foldIn empty swapped
where
  foldIn : HashMap b (List a) -> (b, a) -> HashMap b (List a)
  foldIn accum (k, v) = case lookup k accum of
    Nothing => HashMap.insert k [v]       accum
    Just vs => HashMap.insert k (v :: vs) accum


||| XXX: should be library function
export partial
unwrapLeft : Either a b -> a
unwrapLeft (Left x) = x


||| XXX: should be library function
export partial
unwrapRight: Either a b -> b
unwrapRight (Right y) = y


||| XXX: should be a library function
export
fromMaybe : e -> Maybe a -> Either e a
fromMaybe err Nothing         = Left  err
fromMaybe err (Just whatever) = Right whatever


||| XXX: should be a library function
export
foldDict : Eq a => Ord a => a  -> Nat -> HashMap a Nat -> HashMap a Nat
foldDict k v accum = case HashMap.lookup k accum of
  Nothing => insert k v       accum
  Just x  => insert k (v + x) accum
