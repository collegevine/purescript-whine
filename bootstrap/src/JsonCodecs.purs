module Whine.Bootstrap.JsonCodecs
  ( module R
  ) where

import Data.Codec.JSON (Codec, encode, decode) as R
import Data.Codec.JSON.Common (strMap, string, array) as R
import Data.Codec.JSON.Record (object, objectStrict) as R
