module Formulate.Labelled where

import Data.Functor1 (map1)
import Data.Identity1 (Identity1, mkIdentity1)
import Data.Symbol (class IsSymbol, SProxy)
import Formulate.LBox (LBox, lbox)
import Prim.Row as Row

type Labelled row f = Identity1 (LabelEntry row f)

data LabelEntry row f a = LabelEntry (LBox row a) (f a)

labelled ∷ ∀ lbl a _1 row f. Row.Cons lbl a _1 row ⇒ IsSymbol lbl ⇒ SProxy lbl → f a → Labelled row f
labelled lbl fa = mkIdentity1 (LabelEntry (lbox lbl) fa)

mapLabelled ∷ ∀ row f g. (∀ a b. LBox row a → f a → g b) → Labelled row f → Labelled row g
mapLabelled f = map1 \(LabelEntry lbl fa) → LabelEntry lbl (f lbl fa)
