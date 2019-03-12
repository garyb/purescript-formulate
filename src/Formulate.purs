module Formulate
  ( Def(..)
  , Val(..)
  , label
  , value
  , populate
  , module Exports
  ) where

import Data.Functor1 (class Functor1, map1)
import Data.Identity1 (Identity1(..))
import Data.Leibniz (Leibniz)
import Formulate.LBox (reflect)
import Formulate.Labelled (LabelEntry(..), Labelled, labelled) as Exports
import Formulate.Labelled (LabelEntry(..), Labelled, mapLabelled)
import Formulate.Value (Value, mkValue)
import Formulate.Value (Value(..)) as Exports

newtype Def t a = Def (t (Leibniz a))

newtype Val row t a = Val (t (Value row))

label ∷ ∀ row f. Labelled row f → String
label (Identity1 f) = f \(LabelEntry lbl _) → reflect lbl

value ∷ ∀ row t. Labelled row (Val row t) → t (Value row)
value (Identity1 f) = f \(LabelEntry _ (Val val)) → val

populate ∷ ∀ t row. Functor1 t ⇒ Record row → Labelled row (Def t) → Labelled row (Val row t)
populate state = mapLabelled \lbl (Def tlz) → Val (map1 (mkValue state lbl) tlz)
