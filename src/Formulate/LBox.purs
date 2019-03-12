module Formulate.LBox where

import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Prim.Row (class Cons)

newtype LBox row a = LBox (∀ r. (∀ lbl _1. Cons lbl a _1 row ⇒ IsSymbol lbl ⇒ SProxy lbl → r) → r)

lbox ∷ ∀ lbl a _1 row. Cons lbl a _1 row ⇒ IsSymbol lbl ⇒ SProxy lbl → LBox row a
lbox lbl = LBox \f → f lbl

reflectLabel ∷ ∀ row a. LBox row a → String
reflectLabel (LBox f) = f reflectSymbol
