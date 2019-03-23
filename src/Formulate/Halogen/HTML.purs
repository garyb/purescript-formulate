module Formulate.Halogen.HTML where

import Prelude

import Data.Symbol (class IsSymbol, SProxy)
import Formulate (Def, LabelledDef, labelled)
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Prim.Row as Row

type HTML el row err action = HH.HTML (LabelledDef el row err) action

embed
  ∷ ∀ lbl el row err a action _1
  . Row.Cons lbl a _1 row
  ⇒ IsSymbol lbl
  ⇒ SProxy lbl
  → Def el row err a
  → HTML el row err action
embed lbl = HC.widget <<< labelled lbl
