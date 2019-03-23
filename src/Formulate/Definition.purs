module Formulate.Definition where

import Prelude

import Data.Maybe (Maybe(..))

newtype Definition row err b a = Definition { to ∷ Record row → a → b, from ∷ b → a, error ∷ b → Maybe err }

def ∷ ∀ row err a. Definition row err a a
def = Definition { to: \_ a → a, from: identity, error: const Nothing }
