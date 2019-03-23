module Formulate.Halogen.HTML where

import Prelude

import Data.Symbol (class IsSymbol, SProxy)
import Formulate (Def, LabelledDef, labelled)
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Prim.Row as Row

type HTML el row err i = HH.HTML (LabelledDef el row err) i

embed
  ∷ ∀ lbl el row err a i _1
  . Row.Cons lbl a _1 row
  ⇒ IsSymbol lbl
  ⇒ SProxy lbl
  → Def el row err a
  → HTML el row err i
embed lbl = HC.widget <<< labelled lbl
