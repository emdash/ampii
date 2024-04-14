||| Minimalist terminal UI framework.
|||
||| Editable tabular data.
|||
||| Each row must have the same set of columns.
module TUI.View.Table


import Data.Fin
import Data.Nat
import Data.Vect
import Data.Vect.Quantifiers
import TUI.View
import Util
import Zipper


%default total


data Mode
  = Default
  | EditCell
  | AddRow

public export
record Cell ty where
  view : ty
  {auto impl : View ty}

record Table (tys : Vect k Type) where
  headers : Vect k (String, Nat)
  rows    : Zipper (All Cell tys)
  row     : Nat
  col     : Fin k
  mode    : Table.Mode

