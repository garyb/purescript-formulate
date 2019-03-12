module Example.Component
  ( Form
  , FormPart(..)
  , component
  ) where

import Prelude

import Data.Array as Array
import Data.Int as Int
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..))
import Example.Element (Element(..))
import Formulate (LabelledDef, LabelledVal, Value(..), populateLabelled, reflectLabel, renderLabelled)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type Form row = Array (FormPart row)

data FormPart row
  = Group (Array (LabelledDef row Element))
  | Chunk HH.PlainHTML

data Action row
  = Initialize
  | Receive (Record row)
  | Update (Record row → Record row)

type HTML row slots m = HH.ComponentHTML (Action row) slots m

component ∷ ∀ f row m. Form row → H.Component HH.HTML f (Record row) (Record row) m
component form =
  H.mkComponent
    { initialState: identity
    , render: render form
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        , receive = Just <<< Receive
        }
    }

render ∷ ∀ row slots m. Form row → Record row → HTML row slots m
render parts state =
  HH.form
    [ HP.classes [ H.ClassName "form"] ]
    (renderPart state <$> parts)

renderPart ∷ ∀ row slots m. Record row → FormPart row → HTML row slots m
renderPart state = case _ of
  Group elements →
    HH.div
      [ HP.classes [ H.ClassName "formGroup" ] ]
      (renderElement <<< populateLabelled state <$> elements)
  Chunk html →
    HH.fromPlainHTML html

renderElement ∷ ∀ row slots m. LabelledVal row Element → HTML row slots m
renderElement = renderLabelled \lbl → case _ of
  Label text →
    HH.label
      [ HP.classes [ H.ClassName "formLabel" ]
      , HP.for (reflectLabel lbl)
      ]
      [ HH.text text ]
  Text (Value r) →
    HH.input
      [ HP.classes [ H.ClassName "formInput" ]
      , HP.type_ HP.InputText
      , HP.name (reflectLabel lbl)
      , HP.value r.value
      , HE.onValueInput (Just <<< Update <<< r.update)
      ]
  Integer (Value r) →
    HH.input
      [ HP.classes [ H.ClassName "formInput" ]
      , HP.type_ HP.InputNumber
      , HP.name (reflectLabel lbl)
      , HP.value (show r.value)
      , HE.onValueInput (map (Update <<< r.update) <<< Int.fromString)
      ]
  Select f → f \{ value: Value r, options, print } →
    HH.select
      [ HP.classes [ H.ClassName "formSelect" ]
      , HE.onSelectedIndexChange (map (Update <<< r.update) <<< NEL.index options)
      ]
      $ renderOption print r.value <$> Array.fromFoldable options

renderOption ∷ ∀ row slots m a. (a → String) → a → a → HTML row slots m
renderOption print selected value =
  HH.option
    [ HP.selected (print selected == print value) ]
    [ HH.text (print value) ]

handleAction ∷ ∀ row slots m. Action row → H.HalogenM (Record row) (Action row) slots (Record row) m Unit
handleAction = case _ of
  Initialize →
    H.raise =<< H.get
  Receive st →
    H.put st
  Update f → do
    st ← H.modify f
    H.raise st
