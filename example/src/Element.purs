module Example.Element where

import Prelude

import Data.Functor1 (class Functor1)
import Data.List.NonEmpty (NonEmptyList)
import Formulate (Entry(..), mkEntry)

data Element f
  = Label String
  | Text (f String)
  | Integer (f Int)
  | Select (∀ r. (∀ a. SelectOptions f a → r) → r)

type SelectOptions f a =
  { value ∷ f a
  , options ∷ NonEmptyList a
  , print ∷ a → String
  }

instance functor1Element ∷ Functor1 Element where
  map1 f = case _ of
    Label lbl → Label lbl
    Text a → Text (f a)
    Integer a → Integer (f a)
    Select g → g \options → Select \h → h (options { value = f options.value })

--------------------------------------------------------------------------------

label ∷ ∀ a. String → Entry Element a
label = Entry <<< Label

textInput ∷ Entry Element String
textInput = mkEntry Text

integerInput ∷ Entry Element Int
integerInput = mkEntry Integer

select ∷ ∀ a. (a → String) → NonEmptyList a → Entry Element a
select print options = Entry (Select \f → f { value: identity, print, options })
