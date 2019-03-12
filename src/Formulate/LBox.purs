module Formulate.LBox where

import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Prim.Row (class Cons)

newtype LBox row a = LBox (∀ r. (∀ lbl _1. Cons lbl a _1 row ⇒ IsSymbol lbl ⇒ SProxy lbl → r) → r)

lbox ∷ ∀ lbl a _1 row. Cons lbl a _1 row ⇒ IsSymbol lbl ⇒ SProxy lbl → LBox row a
lbox lbl = LBox \f → f lbl

reflect ∷ ∀ row a. LBox row a → String
reflect (LBox f) = f reflectSymbol
