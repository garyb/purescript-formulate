module Formulate where

import Prelude

import Data.Functor1 (class Functor1, map1)
import Data.Leibniz (Leibniz, coerce, coerceSymm)
import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Prim.Row as Row
import Record as Record

newtype Entry t a = Entry (t (Leibniz a))

mkEntry ∷ ∀ t a. (∀ f. f a → t f) → Entry t a
mkEntry f = Entry (f identity)

--------------------------------------------------------------------------------

newtype Value row a = Value { value ∷ a, update ∷ a → Record row → Record row }

mkValue
  ∷ ∀ lbl a _1 row b
  . IsSymbol lbl
  ⇒ Row.Cons lbl a _1 row
  ⇒ Record row
  → SProxy lbl
  → Leibniz a b
  → Value row b
mkValue state lbl lz =
  Value
    { value: coerce lz (Record.get lbl state)
    , update: Record.set lbl <<< coerceSymm lz
    }

--------------------------------------------------------------------------------

newtype Labelled (row ∷ # Type) (t ∷ (Type → Type) → Type)
  = Labelled (∀ r. (∀ lbl x _1. Row.Cons lbl x _1 row ⇒ IsSymbol lbl ⇒ SProxy lbl → Entry t x → r) → r)

labelled ∷ ∀ t lbl a _1 row. Row.Cons lbl a _1 row ⇒ IsSymbol lbl ⇒ SProxy lbl → Entry t a → Labelled row t
labelled lbl a = Labelled \f → f lbl a

--------------------------------------------------------------------------------

data Valued (row ∷ # Type) (t ∷ (Type → Type) → Type)
  = Valued String (t (Value row))

populate ∷ ∀ t row. Functor1 t ⇒ Record row → Labelled row t → Valued row t
populate state (Labelled g) =
  g \lbl (Entry tlza) → Valued (reflectSymbol lbl) (tlza # map1 (mkValue state lbl))
