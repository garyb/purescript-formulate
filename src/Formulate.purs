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

newtype Def row err t a = Def (t (Definition row err a))

newtype Val row err t a = Val (t (Value row err))

type LabelledDef row err t = Labelled row (Def row err t)

type LabelledVal row err t = Labelled row (Val row err t)

populateLabelled ∷ ∀ t row err. Functor1 t ⇒ Record row → LabelledDef row err t → LabelledVal row err t
populateLabelled state = mapLabelled \lbl (Def def) → Val (map1 (mkValue state lbl) def)

renderLabelled ∷ ∀ row err t r. (∀ a. LBox row a → t (Value row err) → r) → LabelledVal row err t → r
renderLabelled f = unLabelled \lbl (Val a) → f lbl a
