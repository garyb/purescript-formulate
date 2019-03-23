module Example.HTML where

import Prelude

import Data.Symbol (class IsSymbol, SProxy)
import Example.Element (Element)
import Formulate (Def, LabelledDef, labelled)
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Prim.Row as Row

type HTML row err i = HH.HTML (LabelledDef Element row err) i

embed
  ∷ ∀ lbl a _1 row err i
  . Row.Cons lbl a _1 row
  ⇒ IsSymbol lbl
  ⇒ SProxy lbl
  → Def Element row err a
  → HTML row err i
embed lbl = HC.widget <<< labelled lbl
