module Formulate.Value where

import Prelude

import Data.Leibniz (Leibniz, coerce, coerceSymm)
import Formulate.LBox (LBox(..))
import Record as Record

newtype Value row a = Value { value ∷ a, update ∷ a → Record row → Record row }

mkValue ∷ ∀ row a b. Record row → LBox row a → Leibniz a b → Value row b
mkValue state (LBox f) lz = f \lbl →
  Value
    { value: coerce lz (Record.get lbl state)
    , update: Record.set lbl <<< coerceSymm lz
    }
