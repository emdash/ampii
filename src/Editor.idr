||| Handles Editing Partial Recipes
|||
|||
module Editor

import Data.List
import Data.String
import Derive.Prelude
import Data.Either
import Language.JSON

%default total
%language ElabReflection
%hide Prelude.(/) -- so we can use `/` to construct paths without ambiguity


||| A type alias for associative lists.
public export 0 AList  : Type -> Type ; AList a = List (String, a)

namespace Path
  ||| A segment of a path, whether top-down or bottom-up
  public export
  data Segment = Key String | Index Nat
  %runElab derive "Segment" [Eq, Ord]


  ||| The editing state at the leaves of a path
public export
data Cursor
  = Carret Nat
  | Span   Nat Nat
%runElab derive "Cursor" [Eq, Ord, Show]


||| The path type for this API.
|||
||| It is optimized for path construction, with an eye towards
||| keyboard navigation. Modifying the tail of a path is O(1).
public export
data Path
  = End (List Segment)
  | Leaf Cursor (List Segment)
%runElab derive "Path" [Eq, Ord]

namespace Path
  ||| Things that may go wrong when dealing with paths:
  public export
  data Error
    = KeyError   String JSON
    | IndexError Nat    JSON
    | LeafError  Cursor JSON
    | Mismatch   Path   JSON

  ||| Type of fallible operations involving paths
  public export
  Result : Type -> Type
  Result = Either Error


||| A DSL for constructing paths:
|||
||| We introduce `(//)` and override `(/)`, so you can write unix-like
||| paths naturally.
namespace DSL
  public export
  interface Append ty where
    (/) : Path -> ty -> Path

  ||| The empty path
  export root : Path ; root = End []

  ||| Use this prefix operator as a convenient way to start a path.
  export
  (//) : Append ty => ty -> Path
  (//) seg = root / seg

  -- adjust precedence so that no parenthesis are required when followed by `/`
  prefix 10 //

  ||| You can append a string to a path, which indexes an object.
  export
  Append String where
    (/) (End    s) key = End    $ Key key :: s
    (/) (Leaf c s) key = Leaf c $ Key key :: s

  ||| You can to append a number to a path, which indexes an array.
  export
  Append Nat where
    (/) (End    s) i = End    $ Index i :: s
    (/) (Leaf c s) i = Leaf c $ Index i :: s

  ||| You can append a cursor, which changes the variant to Leaf.
  export
  Append Cursor where
    (/) (End    s) c = Leaf c s
    (/) (Leaf _ s) c = Leaf c s

  ||| Appending () changes the path variant to `End`
  export
  Append () where
    (/) (End    s) _ = End s
    (/) (Leaf _ s) _ = End s

  ||| Demonstrate the path DSL, while also testing it. The reversal on
  ||| the right-hand-side is intentional!
  example : (// "foo" / "bar") = (End [Key "bar", Key "foo"])
  example = Refl

  ||| Helper function which stringifies path contents.
  showSubpath : List Path.Segment -> String
  showSubpath = joinBy "/" . map segment . reverse
    where
      segment : Segment -> String
      segment (Key   s) = show s
      segment (Index i) = show i

  ||| Stringify paths using the the DSL notation defined here.
  export
  Show Path where
    show (End         s) = "// \{showSubpath s}"
    show (Leaf cursor s) = "// \{showSubpath s} / {show cursor}"


||| Implementation details are tucked away in this namespace.
namespace Internal
  ||| This is top-down representation, which is optimized for
  ||| operating on data.
  data Path
    = End
    | Leaf Cursor
    | Key String Internal.Path
    | Index Nat  Internal.Path
  %runElab derive "Path" [Eq, Ord, Show]

  ||| Append path segments to an internal path.
  (::) : Internal.Path -> Path.Segment -> Internal.Path
  (::) tail (Key   k) = Key   k tail
  (::) tail (Index i) = Index i tail

  ||| Convert from external to internal representation.
  export
  toInternal : Editor.Path -> Internal.Path
  toInternal (End    xs) = foldl (::) End      xs
  toInternal (Leaf c xs) = foldl (::) (Leaf c) xs

  ||| Convert from internal to external representation.
  toExternal : {default root ret : Editor.Path} -> Internal.Path -> Editor.Path
  toExternal {ret} End                 = ret
  toExternal {ret} (Leaf  cursor)      = ret / cursor
  toExternal {ret} (Key   key subpath) = toExternal {ret = (ret / key)} subpath
  toExternal {ret} (Index i   subpath) = toExternal {ret = (ret / i)}   subpath

  ||| Handle cursor indices at the ends of paths
  indexLeaf : Cursor -> JSON -> Result JSON
  -- XXX: TBD

  ||| Specialize `index` for List JSON
  index : Nat -> List JSON -> Result JSON
  index i l = case inBounds i l of
    Yes _ => Right $ index i l
    No  _ => Left  $ IndexError i (JArray l)

  namespace AList
    ||| Specialize `index` for AList JSON.
    |||
    ||| This is indexing on an object numerically, ignoring its
    ||| key. This makes sense in the context of an interactive editor,
    ||| but not generally.
    export
    index : Nat -> AList JSON -> Result JSON
    index i o = case inBounds i o of
      Yes _ => Right $ snd $ index i o
      No  _ => Left  $ IndexError i (JObject o)

  ||| Specialize `lookup` for `AList` JSON
  lookup : String -> AList JSON -> Path.Result JSON
  lookup key xs = maybeToEither (KeyError key (JObject xs)) $ lookup key xs

  ||| Get the target of the given path from the given data.
  export
  get : Internal.Path -> JSON -> Result JSON
  get End          json         = Right       json
  get (Leaf c)     json         = indexLeaf c json
  get (Key   k sp) (JObject xs) = get sp $ !(lookup k xs)
  get (Index i sp) (JArray  xs) = get sp $ !(index  i xs)
  get (Index i sp) (JObject xs) = get sp $ !(index  i xs)
  get path         json         = Left $ Mismatch (toExternal path) json

  ||| Delete the subtree at the given path in the given data.
  export
  delete : Internal.Path -> JSON -> Result JSON

  ||| Insert a subtree at the given path.
  export
  insert : Internal.Path -> JSON -> JSON -> Result JSON

||| Retrieve the the data referenced by the given path.
export
get : Path -> JSON -> Result JSON
get path = Internal.get (toInternal path)

export
delete : Path -> JSON -> Result JSON
delete path = Internal.delete (toInternal path)

export
insert : Path -> (subtree : JSON) -> JSON -> Result JSON
insert path = Internal.insert (toInternal path)


{-

mutual
  clobberSnd : Path -> (String, JSON) -> Result (String, JSON)
  clobberSnd path (key, value) = Right (key, !(clobber path value))

  clobber : Path -> JSON -> Result JSON
  clobber (Key k End)   (JObject xs) = Right $ JObject $ !(remove k xs)
  clobber (Key str sp)  (JObject xs) = Right $ JObject $ !(traverse (clobberSnd sp) xs)
  clobber (Index i End) (JArray xs)  = Right $ JArray  $ !(delete' i xs)
  clobber (Index k sp)  (JArray xs)  = Right $ JArray  $ !(traverse (clobber sp) xs)
  clobber (Leaf    sp)  json         = Right $ !(clobber sp json)
  clobber End           _            = Right JNull
  clobber path          _            = Left  $ InvalidPath path


test1 : JSON
test1 = JObject [
  ("foo", JBoolean True),
  ("bar", JBoolean False),
  ("baz", JArray [
    JNumber 1,
    JNumber 2,
    JNumber 3
  ])
]

data Schema
  = Number
  | Str
  | Bool   Bool
  | Vec    Nat
  | Tuple  (List Schema)
  | Rec    (AList Schema)
  | Obj    (List String) Schema
  | Option Schema
  | Union  Schema Schema

