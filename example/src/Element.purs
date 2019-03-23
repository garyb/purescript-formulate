module Example.Element where

import Prelude

import Data.Either (Either)
import Data.Functor1 (class Functor1)
import Data.List.NonEmpty (NonEmptyList)
import Formulate (Def(..), def)
import Formulate.Validated (Validated, validated)

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

label ∷ ∀ row err a. String → Def Element row err a
label = Def <<< Label

textInput ∷ ∀ row err. Def Element row err String
textInput = Def (Text def)

textInputV ∷ ∀ row err a. (String → Either err a) → Def Element row err (Validated err String a)
textInputV f = Def (Text (validated f))

integerInput ∷ ∀ row err. Def Element row err Int
integerInput = Def (Integer def)

select ∷ ∀ row err a. (a → String) → NonEmptyList a → Def Element row err a
select print options = Def (Select \f → f { value: def, print, options })
