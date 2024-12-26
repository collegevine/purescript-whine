module Whine.Bootstrap.Log where

import Whine.Prelude

import Codec.JSON.DecodeError as DecodeError
import Effect.Exception as Err

class Loggable a where
  toDoc :: a -> String

instance Loggable String where
  toDoc = identity

instance Loggable Err.Error where
  toDoc = Err.message

instance Loggable DecodeError.DecodeError where
  toDoc = DecodeError.print
