module Formulate
  ( Def(..)
  , Val(..)
  , LabelledDef
  , LabelledVal
  , populateLabelled
  , renderLabelled
  , module Formulate.LBox
  , module Exports
  ) where

import Data.Functor1 (class Functor1, map1)
import Data.Leibniz (Leibniz)
import Formulate.LBox (LBox(..), reflectLabel)
import Formulate.Labelled (LabelEntry(..), Labelled, labelled) as Exports
import Formulate.Labelled (Labelled, mapLabelled, unLabelled)
import Formulate.Value (Value(..)) as Exports
import Formulate.Value (Value, mkValue)

newtype Def t a = Def (t (Leibniz a))

newtype Val row t a = Val (t (Value row))

type LabelledDef row t = Labelled row (Def t)

type LabelledVal row t = Labelled row (Val row t)

populateLabelled ∷ ∀ t row. Functor1 t ⇒ Record row → LabelledDef row t → LabelledVal row t
populateLabelled state = mapLabelled \lbl (Def tlz) → Val (map1 (mkValue state lbl) tlz)

renderLabelled ∷ ∀ row t r. (∀ a. LBox row a → t (Value row) → r) → LabelledVal row t → r
renderLabelled f = unLabelled \lbl (Val a) → f lbl a
