||| Handles Editing Partial Recipes
module Editor

import Data.Vect
import Data.SortedMap
import Data.Maybe
import Language.JSON


%default total
%language ElabReflection


||| ADT for JSON schemas
|||
||| TODO: extend this to a richer set of predicates
public export
data Schema
  = Number
  | Str
  | Array   (Maybe Nat) Schema
  | Record  (SortedMap String Schema)
  | Map     (String -> Bool) Schema
  | Option Schema

public export
any : String -> Bool
any = const True

||| Refine the schema based on field lookup
public export
checkField : String -> Schema -> Maybe Schema
checkField key (Record fields) = lookup key fields
checkField key (Map keys s)    = if keys key then Just s else Nothing
checkField _   _               = Nothing

||| Refine the schema based on numeric index
public export
checkIndex : Nat -> Schema -> Maybe Schema
checkIndex i (Array Nothing  x) = Just (Option x)
checkIndex i (Array (Just y) x) = if i < y then Just x else Nothing
checkIndex _ _                  = Nothing

||| Refine schema for an optional field whose value is present
public export
checkPresence : Schema -> Maybe Schema
checkPresence (Option s) = Just s
checkPresence _          = Nothing


mutual
  ||| A dependent pair whose value depends on the given schema
  record Field where
    constructor F
    schema: Schema
    value: Validated schema

  ||| Helper routine to project out just the schemas from a set of fields
  schemas : SortedMap String Field -> SortedMap String Schema
  schemas fields = map (.schema) fields

  ||| Helper routine which converts a mapping of fields into a predicate
  contains : SortedMap String a -> String -> Bool
  contains map key = case lookup key map of
    Just _ => True
    Nothing => False

  ||| An ADT for validated JSON
  |||
  ||| The shape of the data is guaranteed to match the schema index.
  data Validated : Schema -> Type where
    N : Double               -> Validated Number
    S : String               -> Validated Str
    A : Vect n (Validated s) -> Validated (Array (Just n) s)
    L : List (Validated s)   -> Validated (Array Nothing  s)
    M : (fields : SortedMap String (Validated s)) -> Validated (Map (contains fields) s)
    R : (fields : SortedMap String Field) -> Validated (Record $ schemas field)
    Some : Validated present -> Validated (Option present)
    None : (absent : Schema) -> Validated (Option absent)


||| Data type for a validation error
record Mismatch where
  constructor MM
  expected: Schema
  found: JSON

mutual
  ||| Validate the given JSON against the given schema
  validate : JSON -> (s : Schema) -> Either Mismatch (Validated s)
  validate JNull          (Option s) = Right $ None s
  validate json           (Option s) = map Some (validate json s)
  validate (JBoolean x)   s          = ?validate_rhs_1
  validate (JNumber dbl)  Number     = Right $ N dbl
  validate (JString str)  Str        = Right $ S str
  validate (JArray jsons) s          = validateArray jsons s
  validate (JObject xs)   s          = validateObj xs s
  validate got            expected   = Left $ MM expected got

  validateArray : List JSON -> (s : Schema) -> Either Mismatch (Validated s)
  validateArray jsons (Array Nothing    s) = map L (validateList jsons s)
  validateArray jsons (Array (Just len) s) = map A (validateVect jsons len s)
  validateArray got   expected             = Left $ MM expected (JArray got)

  validateList
    :  List JSON
    -> (s : Schema)
    -> Either Mismatch (List (Validated s))
  validateList []        s = Right $ []
  validateList (x :: xs) s = Right $ !(validate x s) :: !(validateList xs s)

  validateVect
    :  List JSON
    -> (len : Nat)
    -> (s : Schema)
    -> Either Mismatch (Vect len (Validated s))
  validateVect []        Z     s = Right $ []
  validateVect (x :: xs) (S n) s = Right $ !(validate x s) :: !(validateVect xs n s)
  validateVect xs        _     s = Left  $ MM s (JArray xs)

  validateObj
     :  List (String, JSON)
     -> (s : Schema)
     -> Either Mismatch (Validated s)
  validateObj json (Map keys s)    = validateMap json keys s
  validateObj json (Record fields) = validateRecord (fromList json) fields
  validateObj got  expected        = Left $ MM expected (JObject got)

  validateMap
    :  List (String, JSON)
    -> (keys : String -> Bool)
    -> Schema
    -> Either Mismatch (Validated (Map keys s))
  validateMap fields keyset s = ?hole

  validateRecord
    :  SortedMap String JSON
    -> (fields : SortedMap String Schema)
    -> Either Mismatch (Validated (Record fields))


||| A validated path into JSON data
|||
||| The path is indexed by the schema, with the goal that invalid
||| paths are unrepresentable, and so lookup into validated data must
||| succeed.
public export
data Path : Schema -> Type where
  ||| A string index into an object
  Key
    :  (key : String)
    -> {auto 0 valid : IsJust (checkField key s)}
    -> Path (fromJust (checkField key s))
    -> Path s
  ||| A numeric index into a vector, array, or tuple
  Index
    :  (i : Nat)
    -> {auto 0 valid : IsJust (checkIndex key s)}
    -> Path (fromJust (checkIndex key s))
    -> Path s
  ||| Retrive an optional value, or its default.
  Get
    :  {auto 0 _ : IsJust (checkPresence s)}
    -> Validated (fromJust (checkPresence s))
    -> Path (fromJust (checkPresence s))
    -> Path s
  End : Path s


||| Infallible lookup on maps
|||
||| The goal here is to use proofs embedded in the schema to make map
||| lookup always succeed.
safeLookup
  :  (key : String)
  -> (m : SortedMap String a)
  -> (0 hasKey : IsJust (checkField key s))
  -> Validated (fromJust (checkField key s))

||| Retrieve the schema at the end of the path
0 PathSchema : Path s -> Schema
PathSchema (Key   _ sp) = PathSchema sp
PathSchema (Index _ sp) = PathSchema sp
PathSchema (Get   _ sp) = PathSchema sp
PathSchema {s} End      = s


failing "While processing right hand side of lookup."
  public export
  lookup : (p : Path s) -> Validated s -> Validated (PathSchema p)
  lookup (Key key {valid} sp) (M fields) = lookup sp $ safeLookup key fields valid
  lookup (Key key {valid} sp) (R fields) = lookup sp $ safeLookup key fields valid
  lookup (Index i sp) (A items)  = lookup sp (index i items)
  lookup (Get def sp) JNull      = None
  lookup (Get def sp) x          = Some $ lookup sp x
  lookup End          x          = x
