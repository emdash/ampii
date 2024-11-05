||| A "database" that is backed by a filesystem directory.
|||
||| Records are stored as JSON documents under this directory, with
||| the filename derived from the primary key.
module DirDB


import Data.SortedMap
import Data.String
import JSON.FromJSON
import JSON.ToJSON
import JSON.Derive

import System
import System.File
import System.Directory


%default total
%language ElabReflection


||| Type alias for a path-safe string
public export
0 Path : Type
Path = String

||| Type alias for database errors
public export
0 Error : Type
Error = String

||| Interface for converting to a path-safe string
public export
interface Show t => PathSafe t where
  toPath   : t -> Maybe Path
  fromPath : Path -> Maybe t
  showId   : t -> String
  showId   = show

||| XXX: this should do some proper string escaping, but I don't want
||| to follow this rabbit hole right now.
public export
PathSafe String where
  toPath = Just
  fromPath = Just

||| Opaque type alias for database query results
|||
||| For now this is just a wrapper around list.
export
0 Table : Type -> Type -> Type
Table k v = SortedMap k v

||| Interface both primary key and contents must satisfy
public export
interface Show t => FromJSON t => ToJSON t => Serializable t where

||| Blanket implementation, otherwise we'd have to specify it for every type.
public export
implementation Show t => FromJSON t => ToJSON t => Serializable t where

||| Interface for a single row in the database
public export
interface Serializable v => Row v where
  constructor MkRow
  0 PrimaryKey : Type
  primaryKey : v -> PrimaryKey
  OrdPK      : Ord PrimaryKey
  PathSafePK : PathSafe PrimaryKey

||| The associated primary key type, as a function of the row type.
public export
0 (.PrimaryKey) : (v : Type) -> Row v => Type
(.PrimaryKey) v = PrimaryKey {v = v}

||| The associated table type, as a function of the row type.
public export
0 (.Table) : (v : Type) -> Row v => Type
(.Table) v = Table v.PrimaryKey v

||| A predicate on a row type.
public export
0 (.Predicate) : (v : Type) -> Row v => Type
(.Predicate) v = v -> Bool

||| A helper function for showing primary keys
export
showPK : Row v => v.PrimaryKey -> String
showPK self = showId @{PathSafePK} self

||| A helper function to show the primary key type of a row.
export
(.showPK) : Row v => v -> String
(.showPK) self = showId @{PathSafePK} (primaryKey self)

||| Filter a database index with the given predicate.
export
(.where)
  :  Row v
  => v.Table
  -> v.Predicate
  -> v.Table
(.where) row f = fromList @{OrdPK} $ filter (f . snd) $ SortedMap.toList row

||| Order a table of results.
export
(.orderBy)
  : Row v
  => v.Table
  -> (v -> a)
  -> Ord a
  => List v
(.orderBy) row f = sortBy order $ toList row
  where
    order : v -> v -> Ordering
    order a b = compare (f a) (f b)

||| Defines a foreign key relationship between two tables.
|||
||| Client code must specify the the type of the join result, a
||| function which projects the foreign key, and a function which
||| joins the the two values.
public export
interface Row v1 => Row v2 => ForeignKey v1 v2
where
  0 Join : Type
  foreignKey : v1 -> Maybe v2.PrimaryKey
  joinValue  : v1 -> v2 -> Maybe Join

||| Return the table type for the left hand side of a foreign key relation.
public export
0 (.Left) : ForeignKey v1 v2 -> Type
(.Left) fk = v1.Table

||| Return the table type for the right hand side of a foreign key relation.
public export
0 (.Right) : ForeignKey v1 v2 -> Type
(.Right) fk = v2.Table

||| Return the table type for the join resulting from this foreign key relation.
public export
0 (.Joined) : ForeignKey v1 v2 -> Type
(.Joined) fk = Table v1.PrimaryKey (Join @{fk})

||| Helper function which handles joining a single row in the source
||| table.
|||
||| If the foreign key isn't present in its source table, the result
||| is omitted from the join.
joinRow
  :  (fk : ForeignKey v1 v2)
  => fk.Right
  -> fk.Joined
  -> v1
  -> fk.Joined
joinRow right result v1 = fromMaybe result $ go
  where
    go : Maybe fk.Joined
    go = do
      foreign <- foreignKey @{fk} v1
      value   <- lookup foreign right
      joined  <- joinValue v1 value
      Just $ insert (primaryKey v1) joined result

||| Join two tables
export
join
  :  (fk : ForeignKey v1 v2)
  => fk.Left
  -> fk.Right
  -> fk.Joined
join {v1} t1 t2 = foldl {
  func  = joinRow t2,
  init  = empty @{OrdPK},
  input = values t1
}

||| A handle to an open database
public export
record Handle v where
  constructor New
  path: Path

||| Calculate the record's file name from its id
idToPath : Row v => Handle v -> v.PrimaryKey -> Maybe Path
idToPath handle key = do
  path <- toPath @{PathSafePK} key
  pure "\{handle.path}/\{path}.json"

||| Get the path directly from a row object.
(.path) : Row v => v -> Handle v -> Maybe Path
(.path) v handle = idToPath handle $ primaryKey v

||| Inverse of above, except we don't prefix with the path
pathToId : Row v => Path -> Maybe v.PrimaryKey
pathToId path = case toList $ String.split (== '.') path of
  [x, ".json"] => fromPath @{PathSafePK} x
  _            => Nothing

||| Filter out path which don't match the expected pattern
pathsToIds : Row v => List Path -> List v.PrimaryKey
pathsToIds [] = []
pathsToIds (x :: xs) = case pathToId x of
  Nothing => pathsToIds xs
  Just x  => x :: pathsToIds xs

||| Read a single container entry from the inventory directory
export covering
readRow : Row v => Handle v -> v.PrimaryKey -> IO (Either Error v)
readRow handle id = do
  case idToPath handle id of
    Nothing => pure $ Left "Invalid row ID: \{showPK id}"
    Just file => do
      Right contents <- readFile file | Left _ => pure $ Left "Invalid File Name"
      case decode contents of
        Right c => pure $ Right c
        Left  e => pure $ Left "Corrupt record \{showPK id}: \{show e}"

||| Read the given list of containers from the inventory directory
export covering
readRows : Row v => Handle v -> List v.PrimaryKey -> IO (Either Error v.Table)
readRows h [] = pure $ Right $ empty @{OrdPK}
readRows h (id :: ids) = do
  Right cur <- readRow h id
              | Left _ => pure $ Left "Couldn't retrieve \{showPK id}"
  Right rest <- readRows h ids
              | Left _ => pure $ Left "Couldn't read rest of the files"
  pure $ Right $ insert id cur rest

||| Save the given record to the inventory
export covering
writeRow : Row v => Handle v -> v -> IO (Either Error ())
writeRow handle row = do
  case row.path handle of
    Nothing => pure $ Left "Invalid row ID: \{row.showPK}"
    Just file => do
      let contents = encode row
      -- XXX: prevent overwriting existing document
      Right _ <- writeFile file contents | Left err => pure $ Left (show err)
      putStrLn "Saved: \{row.showPK}"
      pure $ Right ()

||| Remove the given row
export covering
deleteRow : Row v => Handle v -> v.PrimaryKey -> IO (Either Error ())
deleteRow handle id = do
  case idToPath handle id of
    Nothing => pure $ Left "Invalid row ID: \{showPK id}"
    Just file => do
      Right _ <- removeFile file | Left _ => pure $ Left "Invalid Path"
      pure $ Right ()

||| Read the entire DB at the given path
export covering
select : Row v => Handle v -> IO (Either Error v.Table)
select handle = do
  let path = handle.path
  Right files <- listDir path | Left _ => pure $ Left "Invalid Path"
  readRows handle $ pathsToIds files

||| Print a table to the console.
public export
print : Row v => v.Table -> IO ()
print rows = for_ (values rows) printRow
  where printRow : v -> IO ()
        printRow row = putStrLn "\{row.showPK}: \{show row}"

||| Check for the existence of the database, returning a handle
export covering
connect
  :  Row v
  => Path
  -> IO (Either Error (Handle v))
connect path = do
  e <- exists path
  case e of
    True  => pure $ Right $ New path
    False => do
      Right _ <- createDir path | Left err => pure $ Left $ show err
      pure $ Left "Directory not found"
