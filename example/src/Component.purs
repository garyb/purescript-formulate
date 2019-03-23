module Example.Component (component) where

import Prelude

import Control.MonadZero (guard)
import Data.Array as Array
import Data.Int as Int
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..), isJust)
import Example.Element (Element(..))
import Formulate (LabelledVal, Value(..), reflectLabel, renderLabelled)
import Formulate.Halogen.Component as FC
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

component
  ∷ ∀ f row action slots m
  . H.Component HH.HTML f (FC.Input Element row String action slots m) (FC.Message row action) m
component = FC.component (\_ → renderElement)

type HTML row action slots m = HH.ComponentHTML (FC.Action row action) slots m

renderElement
  ∷ ∀ row action slots m
  . LabelledVal Element row String
  → HTML row action slots m
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
      , HE.onValueInput (Just <<< FC.Update <<< r.update)
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
      , HE.onValueInput (map (FC.Update <<< r.update) <<< Int.fromString)
      ]
  Select f → f \{ value: Value r, options, print } →
    HH.select
      [ HP.classes $ join
          [ pure $ H.ClassName "formSelect"
          , guard (isJust r.error) $> H.ClassName "invalid"
          ]
      , HE.onSelectedIndexChange (map (FC.Update <<< r.update) <<< NEL.index options)
      ]
      $ renderOption print r.value <$> Array.fromFoldable options

renderOption
  ∷ ∀ row action slots m a
  . (a → String)
  → a
  → a
  → HTML row action slots m
renderOption print selected value =
  HH.option
    [ HP.selected (print selected == print value) ]
    [ HH.text (print value) ]
