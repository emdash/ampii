||| Candidates for Upstream Contributions
|||
||| For a definition to live here it must be sufficiently general that
||| it would ideally live somewhere upstream (e.g. the standard
||| library, prelude, or another some other project).
|||
||| Definitions should be namespaced within this file according to how
||| they would be imported if they existed upstream. E.g., a function
||| on strings intended for the standard library should live in a
||| namespace called `Data.String' in this file.
|||
||| Since the code here is intended to be distributed freely, the code
||| in this file is public domain, in contrast to the AGPL main license.
module Util


import Data.Fin
import Data.SnocList
import Data.String
import Data.SortedMap
import Data.Vect
import Data.Vect.Quantifiers
import Language.JSON


%default total


namespace Data.String
  ||| Truncate a string to the given length.
  public export
  truncateTo : Nat -> String -> String
  truncateTo n s = pack $ take n $ unpack s

  ||| Format a string into an exact width
  |||
  ||| Pad if length is shorter than width, truncate if longer.
  public export
  frameString : Nat -> String -> String
  frameString w s =
    let l = length s
    in case (l < length s) of
      True  => padRight w ' ' s
      False => truncateTo w s

  ||| Append a single character to the given string
  public export
  append : Char -> String -> String
  append char self = self ++ singleton char


namespace Data.SortedMap
  -- xxx: this needs to be more generic.
  export
  foldDict : Eq a => Ord a => a  -> Nat -> SortedMap a Nat -> SortedMap a Nat
  foldDict k v accum = case SortedMap.lookup k accum of
    Nothing => insert k v       accum
    Just x  => insert k (v + x) accum

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
