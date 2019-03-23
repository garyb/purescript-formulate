module Formulate.Halogen.Component
  ( Slot
  , Input
  , Action(..)
  , Message(..)
  , component
  ) where

import Prelude

import Data.Bifunctor (lmap)
import Data.Const (Const)
import Data.Functor1 (class Functor1)
import Formulate (LabelledVal, populateLabelled)
import Formulate.Halogen.HTML (Widget(..))
import Formulate.Halogen.HTML as FH
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core as Core
import Halogen.HTML.Core as HC

type Slot row action = H.Slot (Const Void) (Message row action)

type Input el row err action slots m =
  { form ∷ FH.HTML el row err action slots m
  , state ∷ Record row
  }

data Action row action
  = Update (Record row)
  | Raise action

data Message row action
  = Change (Record row)
  | Bubble action

component
  ∷ ∀ el row err action slots f m
  . Functor1 el
  ⇒ (Record row → LabelledVal el row err → HH.ComponentHTML (Action row action) slots m)
  → H.Component HH.HTML f (Input el row err action slots m) (Message row action) m
component renderElement =
  H.mkComponent
    { initialState: identity
    , render: render renderElement
    , eval
    }

eval
  ∷ ∀ el row err action slots f m
  . H.HalogenQ f (Action row action) (Input el row err action slots m)
  ~> H.HalogenM (Input el row err action slots m) (Action row action) slots (Message row action) m
eval = case _ of
  H.Initialize a → do
    H.raise <<< Change =<< H.gets _.state
    pure a
  H.Finalize a →
    pure a
  H.Receive state a → do
    H.put state
    pure a
  H.Action act a → do
    case act of
      Update state → do
        H.modify_ (_ { state = state })
        H.raise (Change state)
      Raise action → do
        H.raise (Bubble action)
    pure a
  H.Query _ k →
    pure (k unit)

render
  ∷ ∀ el row err action slots m
  . Functor1 el
  ⇒ (Record row → LabelledVal el row err → HH.ComponentHTML (Action row action) slots m)
  → Input el row err action slots m
  → HH.ComponentHTML (Action row action) slots m
render renderElement { form, state } = HC.renderWidget Raise go form
  where
    go = case _ of
      Embed el → renderElement state (populateLabelled state el)
      Slot s → lmap (map Raise) (Core.widget s)
