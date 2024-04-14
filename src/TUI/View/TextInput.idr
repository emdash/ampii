||| An Editable String View
module TUI.TextInput


import Data.String
import TUI.View
import Util
import Zipper


%default total


||| An Editable String View.
export
record TextInput where
  constructor TI
  chars : Zipper Char

||| Construct a text input from a string.
export
fromString : String -> TextInput
fromString s = TI { chars = fromList $ unpack s }

||| get the string value from the text input.
export
toString : TextInput -> String
toString self = pack $ toList self.chars

||| Implement View for TextInput
export
View TextInput where
  -- Size is the sum of left and right halves
  size self = MkArea (length self.chars) 1

  -- when un-focused, just show the string value.
  paint Normal rect self = do
    showTextAt rect.nw (toString self)
  -- when disabled, show a faint string
  paint Disabled rect self = do
    sgr [SetStyle Faint]
    showTextAt rect.nw (toString self)
    sgr [Reset]
  -- when focused, show the cursor position in the string.
  paint Focused rect self = do
    moveTo rect.nw
    putStr $ kcap $ self.chars.left
    reverseVideo
    putStr $ case self.chars.right of
      [] => " "
      x :: _ => singleton x
    unreverseVideo
    putStr $ pack $ tail self.chars.right

  -- map keys to their obvious functions.
  handle Left      = Update . { chars $= goLeft  }
  handle Right     = Update . { chars $= goRight }
  handle Delete    = Update . { chars $= delete  }
  handle (Alpha c) = Update . { chars $= insert c}
  handle Enter     = const    FocusParent
  handle Escape    = const    FocusParent
  handle Tab       = const    FocusNext
  handle _         = Update . id
