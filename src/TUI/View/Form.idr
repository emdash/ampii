||| Minimalist terminal UI framework.
|||
||| Editing and navigation for a set of labeled subviews.
module TUI.View.Form

import public Data.Vect
import public Data.Vect.Quantifiers

import TUI.View
import Util


%default total


||| A single field in a form.
|||
||| It is a labeled value that wraps an inner view, capturing its
||| View implementation.
public export
record Field ty where
  constructor F
  label : String
  view : ty
  {auto impl : View ty}

||| Get the area of the field's wrapped view, not including its
||| label.
viewSize : Field ty -> Area
viewSize self = size @{self.impl} self.view

||| Update field's wrapped view in response to a key event.
handleView : Key -> Field ty -> Response (Field ty)
handleView k f = case handle @{f.impl} k (f.view) of
  Update new  => Update $ { view := new } f
  FocusParent => FocusParent
  FocusNext   => FocusNext

||| A form displays a set of views, each with a string label.
|||
||| One field has focus, and user input is routed to this sub-view.
export
record Form (tys : Vect k Type) where
  constructor MkForm
  fields : All Field tys
  focused : Fin k
  split : Nat
  editing : Bool
  contentSize : Area

parameters {k : Nat} {tys : Vect k Type}
  ||| Get the character width of the longest label in the form.
  maxLabelWidth : All Field tys -> Nat
  maxLabelWidth tys = reduceAll max (length . (.label)) 0 tys

  ||| Calculate the size the form widgets (not including the labels)
  export
  sizeViewsVertical : All Field tys -> Area
  sizeViewsVertical fields = reduceAll hunion (viewSize) (MkArea 0 0) fields

||| Render the form's fields vertically.
export
paintVertical
  : {k : Nat}
  -> {tys : Vect k Type}
  -> State
  -> Rect
  -> Form tys -> IO ()
paintVertical state window self = do
  loop 0 window self.fields
  where
    loop
      : {k : Nat}
      -> {tys : Vect k Type}
      -> Nat
      -> Rect
      -> All Field tys -> IO ()
    loop _  _ [] = pure ()
    loop i  window (x :: xs) = do
      let (top, bottom) = vsplit window (viewSize x).height
      let (left, right) = hsplit top    (self.split + 3)
      let left  = inset left  (MkArea 1 0)
      let right = inset right (MkArea 1 0)
      case (i == (finToNat self.focused), state) of
        (True, Focused) => do
          if self.editing
            then sgr [SetStyle SingleUnderline]
            else reverseVideo
          showTextAt left.nw x.label
          sgr [Reset]
          if self.editing
            then paint @{x.impl} Focused right x.view
            else paint @{x.impl} Normal  right x.view
        _ => do
          showTextAt left.nw x.label
          paint @{x.impl} Normal right x.view
      loop (S i) bottom xs

||| Dispatch keyboard input to the currently-focused subview.
|||
||| This has to handle `Escape`ing if the subview escapes.
export
handleNth
  : {k : Nat}
  -> {tys : Vect k Type}
  -> Fin k
  -> Key
  -> All Field tys
  -> Response (All Field tys)
handleNth FZ key (f :: fs) = case handleView key f of
  Update new  => Update $ new :: fs
  FocusParent => FocusParent
  FocusNext   => FocusNext
handleNth (FS i) key (f :: fs) = case handleNth i key fs of
  Update fs => Update $ f :: fs
  FocusParent => FocusParent
  FocusNext  => FocusNext

parameters {k : Nat} {tys : Vect k Type}
  ||| Move the form to the next focused value.
  export
  nextChoice : Form tys -> Form tys
  nextChoice = { focused $= finS }

  ||| Move the form to the previous focused value.
  export
  prevChoice : Form tys -> Form tys
  prevChoice = { focused $= predS }

  ||| Dispatch event to the selected field.
  |||
  ||| We may need to update our editing state in response.
  export
  handleEditing : Key -> Form tys -> Response (Form tys)
  handleEditing key self = case handleNth self.focused key self.fields of
    Update fields => Update $ { fields  := fields } self
    FocusParent   => Update $ { editing := False }  self
    FocusNext     => Update $ nextChoice            self

  ||| Handles events when in navigation mode.
  |||
  ||| Up/Down change the form focus, various other keys toggle the
  ||| editing state.
  export
  handleDefault : Key -> Form tys -> Response (Form tys)
  -- handleDefault Up _ impossible
  handleDefault Up     = Update . prevChoice
  handleDefault Down   = Update . nextChoice
  handleDefault Tab    = Update . nextChoice
  handleDefault Right  = Update . { editing := True }
  handleDefault Enter  = Update . { editing := True }
  handleDefault Escape = const FocusParent
  handleDefault Left   = const FocusParent
  handleDefault _      = Update . id

||| The View implementation for form renders each labeled sub-view
||| vertically.
|||
||| The labels are left-justified, while the sub-views are
||| left-justified, and aligned relative to the widest label in the
||| form.
|||
||| Only one sub-view has focus. Tab is used to move focus to the
||| next form field.
export
{k : Nat} -> {tys : Vect (S k) Type} -> View (Form tys) where
  size self = self.contentSize + MkArea self.split 1

  paint state window self = do
    let height = window.size.height `minus` 2
    vline (window.nw + MkArea 0 1)                height
    vline (window.nw + MkArea (self.split + 3) 1) height
    case state of
      Focused => box window
      _       => pure ()
    paintVertical state (shrink window) self

  -- dispatch events depending on editing state
  handle key self = case self.editing of
    True => handleEditing key self
    False => handleDefault key self


||| Construct a form from a list of field records
public export
form
  : {k : Nat}
  -> {tys : Vect (S k) Type}
  -> All Field tys
  -> Form tys
form fields = MkForm {
  fields      = fields,
  focused     = 0,
  split       = (maxLabelWidth fields),
  contentSize = (sizeViewsVertical fields),
  editing     = False
}
