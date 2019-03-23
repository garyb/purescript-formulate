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
import Formulate.Definition (Definition)
import Formulate.Definition (Definition(..), def) as Exports
import Formulate.LBox (LBox(..), reflectLabel)
import Formulate.Labelled (LabelEntry(..), Labelled, labelled) as Exports
import Formulate.Labelled (Labelled, mapLabelled, unLabelled)
import Formulate.Value (Value(..)) as Exports
import Formulate.Value (Value, mkValue)

newtype Def t row err a = Def (t (Definition row err a))

newtype Val t row err a = Val (t (Value row err))

type LabelledDef t row err = Labelled row (Def t row err)

type LabelledVal t row err = Labelled row (Val t row err)

populateLabelled ∷ ∀ t row err. Functor1 t ⇒ Record row → LabelledDef t row err → LabelledVal t row err
populateLabelled state = mapLabelled \lbl (Def def) → Val (map1 (mkValue state lbl) def)

renderLabelled ∷ ∀ t row err r. (∀ a. LBox row a → t (Value row err) → r) → LabelledVal t row err → r
renderLabelled f = unLabelled \lbl (Val a) → f lbl a
