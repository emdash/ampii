||| An interactive view which represents and exclusive choice.
module TUI.View.Menu


import TUI.View
import Data.Vect
import Util


%default total


||| An interactive view which represents an exclusive choice.
|||
||| XXX: rename me
||| this should be a "spinner" or "chooser", or "combo"
|||
||| The choice set is of a single uniform type, unlike with form.
record Menu a where
  constructor MkMenu
  n       : Nat
  choices : Vect n a
  choice  : Fin  n

-- bind names to some unicode symbols we use.
upArrow     : String ; upArrow     = "⬆"
downArrow   : String ; downArrow   = "⬇"
upDownArrow : String ; upDownArrow = "⬍"

||| Show the arrow indicator most appropriate for the given as a
||| hint to the user which keys will be effective.
|||
||| - 0     : down arrow
||| - last  : the up arrow
||| - other : the up-down arrow.
arrowForIndex : {k : Nat} -> Fin k -> String
arrowForIndex FZ     = downArrow
arrowForIndex (FS n) =
  if FS n == last
  then upArrow
  else upDownArrow

||| View implementation for Menu
|||
||| Up / Down is used to cycle through alternatives.
||| TBD: filter options based on text typed.
export
View a => View (Menu a) where
  size self =
    let width = foldl max 0 $ map (width . size) self.choices
    in MkArea (width + 2) $ height $ size $ index self.choice self.choices

  paint state rect self = do
    case state of
      Focused => reverseVideo
      Disabled => sgr [SetStyle Faint]
      _ => sgr [Reset]
    paint state (snd $ hsplit rect 2) (index self.choice self.choices)
    showTextAt rect.nw (arrowForIndex {k = self.n} self.choice)
    sgr [Reset]

  handle Up     state = Update $ { choice := predS state.choice } state
  handle Down   state = Update $ { choice := finS  state.choice } state
  handle Escape state = FocusParent
  handle Enter  state = FocusParent
  handle Left   state = FocusParent
  handle Tab    state = FocusNext
  handle _      state = Update state

||| Construct a menu from a vector of views
export
menu : {k : Nat} -> View a => Vect (S k) a -> Menu a
menu {k} choices = MkMenu (S k) choices (natToFinLt 0)
