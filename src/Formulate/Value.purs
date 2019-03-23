module Formulate.Value where

import Data.Maybe (Maybe)
import Formulate.Definition (Definition(..))
import Formulate.LBox (LBox(..))
import Record as Record

newtype Value row err a = Value { value ∷ a, update ∷ a → Record row, error ∷ Maybe err }

mkValue ∷ ∀ row err a b. Record row → LBox row a → Definition row err a b → Value row err b
mkValue state (LBox f) (Definition def) = f \lbl →
  let
    value = Record.get lbl state
    update x = Record.set lbl (def.to state x) state
  in
    Value { value: def.from value, update, error: def.error value }
