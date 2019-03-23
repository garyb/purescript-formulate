module Example.Component (component) where

import Prelude

import Control.MonadZero (guard)
import Data.Array as Array
import Data.Int as Int
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..), isJust)
import Example.Element (Element(..))
import Formulate (LabelledVal, Value(..), populateLabelled, reflectLabel, renderLabelled)
import Formulate.Halogen.HTML as FH
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

data Action row
  = Initialize
  | Receive (Record row)
  | Update (Record row)

type HTML row slots m = HH.ComponentHTML (Action row) slots m

component ∷ ∀ f row m. FH.HTML Element row String Void → H.Component HH.HTML f (Record row) (Record row) m
component def =
  H.mkComponent
    { initialState: identity
    , render: render def
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        , receive = Just <<< Receive
        }
    }

render ∷ ∀ row slots m. FH.HTML Element row String Void → Record row → HTML row slots m
render def state =
  HC.renderWidget absurd (renderElement <<< populateLabelled state) def

renderElement ∷ ∀ row slots m. LabelledVal Element row String → HTML row slots m
renderElement = renderLabelled \lbl → case _ of
  Label text →
    HH.label
      [ HP.classes [ H.ClassName "formLabel" ]
      , HP.for (reflectLabel lbl)
      ]
      [ HH.text text ]
  Text (Value r) →
    HH.input
      [ HP.classes $ join
          [ pure $ H.ClassName "formInput"
          , guard (isJust r.error) $> H.ClassName "invalid"
          ]
      , HP.type_ HP.InputText
      , HP.name (reflectLabel lbl)
      , HP.value r.value
      , HE.onValueInput (Just <<< Update <<< r.update)
      ]
  Integer (Value r) →
    HH.input
      [ HP.classes $ join
          [ pure $ H.ClassName "formInput"
          , guard (isJust r.error) $> H.ClassName "invalid"
          ]
      , HP.type_ HP.InputNumber
      , HP.name (reflectLabel lbl)
      , HP.value (show r.value)
      , HE.onValueInput (map (Update <<< r.update) <<< Int.fromString)
      ]
  Select f → f \{ value: Value r, options, print } →
    HH.select
      [ HP.classes $ join
          [ pure $ H.ClassName "formSelect"
          , guard (isJust r.error) $> H.ClassName "invalid"
          ]
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
  Update state → do
    H.put state
    H.raise state
