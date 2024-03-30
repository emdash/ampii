||| A "database" that is backed by a filesystem directory.
|||
||| Records are stored as JSON documents under this directory, with
||| the filename derived from the primary key.
module DirDB


import Data.List1
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
interface PathSafe t where
  toPath   : t -> Path
  fromPath : Path -> t


||| XXX: this should do some proper string escaping, but I don't want
||| to follow this rabbit hole right now.
public export
PathSafe String where
  toPath = id
  fromPath = id


||| Interface both primary key and contents must satisfy
public export
interface Show t => FromJSON t => ToJSON t => Serializable t where


||| Blanket implementation, otherwise we'd have to specify it for every type.
public export
implementation Show t => FromJSON t => ToJSON t => Serializable t where


||| Interface for a single row in the database
public export
interface PathSafe k => Ord k => Serializable k => Serializable v => Row k v where


||| Type alias to construct the row tuple type
public export
0 RowT : (k : Type) -> (v : Type) -> Row k v => Type
RowT k v = (k, v)


||| Type alias for database query results
public export
0 Table : (k : Type) -> (v : Type) -> Row k v => Type
Table k v = SortedMap k v


||| Defines a foreign key relationship between two tables.
|||
||| Client code must specify the the type of the join result, a
||| function which projects the foreign key, and a function which
||| joins the the two values.
public export
interface
  Row k1 v1 =>
  Row k2 v2 =>
  ForeignKey k1 v1 k2 v2
where
  0 JoinResult : Type
  foreignKey : v1 -> k2
  joinValue  : v1 -> v2 -> JoinResult


||| Helper function which handles joining a single row in the source
||| table.
|||
||| If the foreign key isn't present in its source table, the result
||| is omitted from the join.
joinRow
  :  (fk : ForeignKey k1 v1 k2 v2)
  => SortedMap k2 v2
  -> SortedMap k1 (JoinResult @{fk})
  -> (k1, v1)
  -> SortedMap k1 (JoinResult @{fk})
joinRow source dest (k, v1) =
  case SortedMap.lookup (foreignKey @{fk} v1) source of
    Nothing => dest
    Just v  => insert k (joinValue v1 v) dest


||| Join two tables together.
joinTable
  :  (fk : ForeignKey k1 v1 k2 v2)
  => Table k1 v1
  -> Table k2 v2
  -> SortedMap k1 (JoinResult @{fk})
joinTable t1 t2 = foldl {
  func  = joinRow t2,
  init  = empty,
  input = toList t1
}


||| A handle to an open database
public export
record Handle (k : Type) (v : Type) where
  constructor New
  path: Path


||| Calculate the record's file name from its id
idToPath : Row k v => Handle k v -> k -> Path
idToPath handle key = "\{handle.path}/\{toPath key}.json"


||| Inverse of above, except we don't prefix with the path
pathToId : Row k v => Path -> Maybe k
pathToId path = case toList $ String.split (== '.') path of
  [x, ".json"] => Just (fromPath x)
  _            => Nothing


||| Filter out path which don't match the expected pattern
pathsToIds : Row k v => List Path -> List k
pathsToIds [] = []
pathsToIds {k, v} (x :: xs) = case pathToId x {k, v} of
  Nothing => pathsToIds xs {k, v}
  Just x  => x :: pathsToIds xs {k, v}


||| Read a single container entry from the inventory directory
export covering
readRow : Row k v => Handle k v -> k -> IO (Either Error v)
readRow {k, v} handle id = do
  let file = idToPath handle id {k, v}
  Right contents <- readFile file | Left _ => pure $ Left "Invalid File Name"
  case decode contents of
    Right c => pure $ Right c
    Left  e => pure $ Left "Corrupt record \{show id}: \{show e}"


||| Read the given list of containers from the inventory directory
export covering
readRows : Row k v => Handle k v -> List k -> IO (Either Error (Table k v))
readRows h [] = pure $ Right empty
readRows h (id :: ids) = do
  Right cur <- readRow h id
              | Left _ => pure $ Left "Couldn't retrieve \{show id}"
  Right rest <- readRows h ids
              | Left _ => pure $ Left "Couldn't read rest of the files"
  pure $ Right $ insert id cur rest


||| Save the given record to the inventory
public export covering
writeRow : Row k v => Handle k v -> k -> v -> IO (Either Error ())
writeRow handle id contents {k, v} = do
  let file     = idToPath handle id {k, v}
  let contents = encode contents
  -- XXX: prevent overwriting existing document
  Right _ <- writeFile file contents | Left err => pure $ Left (show err)
  putStrLn "Saved: \{show id}"
  pure $ Right ()


||| Remove the given row
public export covering
deleteRow : Row k v => Handle k v -> k -> IO (Either Error ())
deleteRow handle id = do
  let file = idToPath handle id {k, v}
  Right _ <- removeFile file | Left _ => pure $ Left "Invalid Path"
  pure $ Right ()


||| Read the entire DB at the given path
public export covering
readTable : Row k v => Handle k v -> IO (Either Error (Table k v))
readTable handle = do
  let path = handle.path
  Right files <- listDir path | Left _ => pure $ Left "Invalid Path"
  readRows handle $ pathsToIds files {k,v}


||| Read the entire DB, and call `f` with the result
public export covering
withTable
  :  Row k v
  => Handle k v
  -> (Table k v -> r)
  -> IO (Either Error r)
withTable handle f = pure $ map f !(readTable handle)


||| Filter a database index with the given predicate.
filterTable
  :  Row k v
  => (k -> v -> Bool)
  -> Table k v
  -> Table k v
filterTable f index = fromList $ filter (uncurry f) $ toList index


||| Print a set of rows to the console
public export
printRows : Row k v => Table k v -> IO ()
printRows rows = for_ (SortedMap.toList rows) printRow
  where printRow : (k, v) -> IO ()
        printRow (id, row) = putStrLn "\{show id}: \{show row}"


||| Read the DB, returning those items which satisfy the predicate.
export covering
select
  :  Row k v
  => (k -> v -> Bool)
  -> Handle k v
  -> IO (Either Error (Table k v))
select predicate handle = withTable handle (filterTable predicate)


||| Join two tables for which a foreign key relationship is defined.
export covering
join
  :  (fk : ForeignKey k1 v1 k2 v2)
  => Row k1 v1
  => Row k2 v2
  => (k1 -> JoinResult @{fk} -> Bool)
  -> Handle k1 v1
  -> Handle k2 v2
  -> IO (Either Error (SortedMap k1 (JoinResult @{fk})))
join f h1 h2 = do
  Right t1 <- readTable h1 | Left err => pure $ Left err
  Right t2 <- readTable h2 | Left err => pure $ Left err
  pure $ Right $ fromList $ filter (uncurry f) $ toList $ joinTable t1 t2


||| Check for the existence of the database, returning a handle
export covering
connect
  :  Path
  -> IO (Either Error (Handle k v))
connect path = do
  e <- exists path
  case e of
    True  => pure $ Right $ New path
    False => do
      Right _ <- createDir path | Left err => pure $ Left $ show err
      pure $ Left "Directory not found"
