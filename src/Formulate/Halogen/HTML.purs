module Formulate.Halogen.HTML where

import Prelude

import Data.Maybe (Maybe)
import Data.Symbol (class IsSymbol, SProxy)
import Formulate (Def, LabelledDef, labelled)
import Halogen as H
import Halogen.Component (ComponentSlot(..))
import Halogen.HTML as HH
import Halogen.HTML.Core as Core
import Prim.Row as Row

type HTML el row err action slots m = HH.HTML (Widget el row err action slots m) action

data Widget el row err action slots m
  = Embed (LabelledDef el row err)
  | Slot (ComponentSlot HH.HTML slots m action)

embed
  ∷ ∀ lbl el row err a action _1 slots m
  . Row.Cons lbl a _1 row
  ⇒ IsSymbol lbl
  ⇒ SProxy lbl
  → Def el row err a
  → HTML el row err action slots m
embed lbl def = Core.widget (Embed (labelled lbl def))

slot
  :: forall el row err query action input output slots m label slot _1
   . Row.Cons label (H.Slot query output slot) _1 slots
  => IsSymbol label
  => Ord slot
  => SProxy label
  -> slot
  -> H.Component HH.HTML query input output m
  -> input
  -> (output -> Maybe action)
  -> HTML el row err action slots m
slot label p component input outputQuery =
  Core.widget (Slot (ComponentSlot (H.componentSlot label p component input outputQuery)))
