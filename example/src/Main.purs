module Example.Main where

import Prelude

import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Example.Component (component)
import Example.Element (integerInput, label, select, textInput)
import Example.HTML as FH
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

data Language
  = PureScript
  | JavaScript

languageOptions ∷ NEL.NonEmptyList Language
languageOptions = NEL.cons PureScript (NEL.singleton JavaScript)

printLanguage ∷ Language → String
printLanguage = case _ of
  PureScript → "PureScript"
  JavaScript → "JavaScript"

--------------------------------------------------------------------------------

type FormElements =
  ( name ∷ String
  , age ∷ Int
  , language ∷ Language
  )

_name = SProxy ∷ SProxy "name"
_age = SProxy ∷ SProxy "age"
_language = SProxy ∷ SProxy "language"

form ∷ FH.HTML FormElements Void
form =
  HH.form
    [ HP.classes [ H.ClassName "form"] ]
    [ HH.div
        [ HP.classes [ HH.ClassName "field"] ]
        [ FH.embed _name (label "Name")
        , FH.embed _name textInput
        ]
    , HH.div
        [ HP.classes [ HH.ClassName "field"] ]
        [ FH.embed _age (label "Age")
        , FH.embed _age integerInput
        ]
    , HH.div
        [ HP.classes [ HH.ClassName "field"] ]
        [ FH.embed _language (label "Preferred language")
        , FH.embed _language (select printLanguage languageOptions)
        ]
    ]

--------------------------------------------------------------------------------

ui ∷ ∀ f i o m. H.Component HH.HTML f i o m
ui =
  H.mkComponent
    { initialState: \_ → { name: "", age: 18, language: PureScript }
    , render
    , eval: H.mkEval (H.defaultEval { handleAction = H.put })
    }
  where
    render state =
      HH.div_
        [ HH.slot (SProxy ∷ _ "form") unit (component form) state Just
        , HH.p_ [ HH.text ("Name: " <> state.name) ]
        , HH.p_ [ HH.text ("Age: " <> show state.age) ]
        , HH.p_ [ HH.text ("Language preference: " <> printLanguage state.language) ]
        ]

--------------------------------------------------------------------------------

main ∷ Effect Unit
main = HA.runHalogenAff do
  body ← HA.awaitBody
  runUI ui unit body
