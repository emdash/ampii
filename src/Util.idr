module Util

import Data.SnocList
import Data.String
import Data.SortedMap
import Language.JSON


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
transpose
  :  Eq a
  => Eq b
  => Ord a
  => Ord b
  => SortedMap a b
  -> SortedMap b (List a)
transpose dict =
  -- XXX: I spent way too much time trying to do this without creating
  -- an intermediate list. How to avoid this?
  let swapped = map swap (SortedMap.toList dict)
  in foldl foldIn empty swapped
where
  foldIn : SortedMap b (List a) -> (b, a) -> SortedMap b (List a)
  foldIn accum (k, v) = case lookup k accum of
    Nothing => SortedMap.insert k [v]       accum
    Just vs => SortedMap.insert k (v :: vs) accum


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
foldDict : Eq a => Ord a => a  -> Nat -> SortedMap a Nat -> SortedMap a Nat
foldDict k v accum = case SortedMap.lookup k accum of
  Nothing => insert k v       accum
  Just x  => insert k (v + x) accum


namespace Data.SnocList
  ||| `unpack`, but for SnocLists
  public export
  kcapnu : String -> SnocList Char
  kcapnu s = cast $ unpack s

  ||| `pack`, but for SnocLists
  public export
  kcap : SnocList Char -> String
  kcap s = pack $ cast s

  ||| `tail` for SnocList
  public export
  liat : SnocList a -> SnocList a
  liat [<] = [<]
  liat (xs :< x) = xs

  ||| `head` for SnocList
  public export
  daeh : SnocList a -> Maybe a
  daeh [<] = Nothing
  daeh (xs :< x) = Just x

  ||| Return the tail part of a regular list.
  public export
  tail : List a -> List a
  tail [] = []
  tail (x :: xs) = xs
