module Formulate.Validated where

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Formulate.Definition (Definition(..))

data Validated err a b
  = Initial a
  | Validated a (Either err b)

validated ∷ ∀ row err a b. (a → Either err b) → Definition row err (Validated err a b) a
validated f =
  Definition
    { to: \_ a → Validated a (f a)
    , from: case _ of
        Initial a → a
        Validated a _ → a
    , error: case _ of
        Validated _ (Left err) → Just err
        _ → Nothing
    }
