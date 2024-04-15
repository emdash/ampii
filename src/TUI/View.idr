||| Minimalist terminal UI framework.
|||
||| A View is an interactive interface component. It knows how to
||| paint itself, and it knows how to update itself in response to
||| user input. It also can send signals upstream to the application.
|||
||| XXX: Should it be renamed to `Widget` or something similar?
module TUI.View


import public TUI.Event
import public TUI.Painting


||| The drawing state of a view.
|||
||| This is used by the `paint` method to provide appropriate
||| feedback.
public export
data State = Normal | Focused | Disabled

||| A response to an input event.
|||
||| This is returned by the `handle` method, and covers the possible
||| actions supported by the framework.
|||
||| Update      : set view to to the given state.
||| FocusParent : request focus be moved to the parent view.
||| FocusNext   : request focus be moved to the next sibling view.
||| Run         : trigger non-local action after this iteration.
|||
||| `Run` is a way for a view to global, application-specific
||| effects. The TUI framework is agnostic about the type: it's under
||| the control of your application. If you use `runView`, it will
||| dispatch actions to the handler you supply with the top-level
||| state for context.
public export
data Response state action
  = Update state
  | FocusParent
  | FocusNext
  | Run action

||| A view is a high-level UI component.
|||
||| - It wraps an inner value, its state.
||| - It knows how to size itself, for layout purposes.
||| - It can draw itself to the screen
||| - It can update its state in response to events.
public export
interface View state where
  ||| Calculate the "requested" size
  size  : state -> Area

  ||| Draw the view into the given screen rectangle.
  paint : View.State -> Rect -> state -> IO ()

  ||| Possibly update our state in response to a key press.
  |||
  ||| The default implementation just shifts focus, depending on the
  ||| key-press.
  handle : Key -> state -> Response state action
  handle Tab _ = FocusNext
  handle _   _ = FocusParent

||| Implement `View` for `()` as a no-op
export
View () where
  size  _     = MkArea 0 0
  paint _ _ _ = pure ()

||| Any type implementing `Show` is automatically a (non-interative)
||| view.
export
Show a => View a where
  size s = MkArea (length (show s)) 1
  paint _ r s = showTextAt r.nw (show s)

||| In implementing `View` for all `Show` types, we have
||| inadvertently made it ambigious what to do when we use a string
||| as a view. This alternative, named implementation draws the
||| string directly to the screen.
export
[string] View String where
  size s = MkArea (length s) 1
  paint _ r = showTextAt r.nw
