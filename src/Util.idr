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


namespace Data.Either
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


namespace Data.Fin
  ||| Decrement the given `Fin` without changing the bound.
  public export
  predS : {n : Nat} -> Fin n -> Fin n
  predS FZ     = last
  predS (FS k) = weaken k


namespace Data.Nat
  ||| The absolute value of the difference between two Nats
  public export
  diff : Nat -> Nat -> Nat
  diff a b = case a < b of
    True  => b `minus` a
    False => a `minus` b


namespace Data.Vect.Quantifiers
  ||| Map a function over a heterogenous vector.
  |||
  ||| Unlike mapProperty, the result is homogenous vector. This is
  ||| useful for extracting properties which don't vary with index
  ||| type.
  public export
  mapAll
    :  {k : Nat}
    -> {xs : Vect k a}
    -> (f : forall x. p x -> y)
    -> All p xs
    -> Vect k y
  mapAll f xs = forget $ mapProperty f xs

  ||| Fold over a heterogenous vector.
  |||
  ||| The result is collected into a single value.
  ||| f : map each generic value into a single concrete value
  ||| g : accumulate concrete values into a single result.
  public export
  reduceAll
    :  {k : Nat}
    -> {xs : Vect k a}
    -> (g : y -> y -> y)
    -> (f : forall x. p x -> y)
    -> y
    -> All p xs
    -> y
  reduceAll g f accum [] = accum
  reduceAll g f accum (x :: xs) = reduceAll g f (g accum (f x)) xs

  ||| Update the value at the index, without changing the type.
  public export
  updateAt
    :  {k : Nat}
    -> {xs : Vect k a}
    -> (i : Fin k)
    -> (f : forall x. p x -> p x)
    -> All p xs
    -> All p xs
  updateAt FZ     f (x :: xs) = f x :: xs
  updateAt (FS i) f (x :: xs) = x :: updateAt i f xs
