module Formulate.Halogen.Component
  ( Slot
  , Action(..)
  , Message(..)
  , component
  ) where

import Prelude

import Data.Const (Const)
import Data.Functor1 (class Functor1)
import Formulate (LabelledVal, populateLabelled)
import Formulate.Halogen.HTML as FH
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core as HC

type Slot row action = H.Slot (Const Void) (Message row action)

data Action row action
  = Update (Record row)
  | Raise action

data Message row action
  = Change (Record row)
  | Bubble action

component
  ∷ ∀ el row err action slots f m
  . Functor1 el
  ⇒ (LabelledVal el row err → HH.ComponentHTML (Action row action) slots m)
  → FH.HTML el row err action
  → H.Component HH.HTML f (Record row) (Message row action) m
component renderElement def =
  H.mkComponent
    { initialState: identity
    , render: render renderElement def
    , eval
    }

eval
  ∷ ∀ row action slots f m
  . H.HalogenQ f (Action row action) (Record row)
  ~> H.HalogenM (Record row) (Action row action) slots (Message row action) m
eval = case _ of
  H.Initialize a → do
    H.raise <<< Change =<< H.get
    pure a
  H.Finalize a →
    pure a
  H.Receive state a → do
    H.put state
    pure a
  H.Action act a → do
    case act of
      Update state → do
        H.put state
        H.raise (Change state)
      Raise action → do
        H.raise (Bubble action)
    pure a
  H.Query _ k →
    pure (k unit)

render
  ∷ ∀ el row err action slots m
  . Functor1 el
  ⇒ (LabelledVal el row err → HH.ComponentHTML (Action row action) slots m)
  → FH.HTML el row err action
  → Record row
  → HH.ComponentHTML (Action row action) slots m
render renderElement def state =
  HC.renderWidget Raise (renderElement <<< populateLabelled state) def
