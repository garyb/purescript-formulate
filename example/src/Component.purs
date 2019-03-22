module Example.Component (component) where

import Prelude

import Data.Array as Array
import Data.Int as Int
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..))
import Example.Element (Element(..))
import Example.HTML as FH
import Formulate (LabelledVal, Value(..), populateLabelled, reflectLabel, renderLabelled)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

data Action row
  = Initialize
  | Receive (Record row)
  | Update (Record row → Record row)

type HTML row slots m = HH.ComponentHTML (Action row) slots m

component ∷ ∀ f row m. FH.HTML row Void → H.Component HH.HTML f (Record row) (Record row) m
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

render ∷ ∀ row slots m. FH.HTML row Void → Record row → HTML row slots m
render def state =
  HC.renderWidget absurd (renderElement <<< populateLabelled state) def

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
