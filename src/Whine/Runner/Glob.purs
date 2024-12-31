module Whine.Runner.Glob where

import Whine.Runner.Prelude

type Globs = { include :: Array NonEmptyString, exclude :: Array NonEmptyString }

foreign import glob :: Globs -> Effect (Array FilePath)

foreign import test :: Globs -> FilePath -> Boolean

isEmptyGlobs :: Globs -> Boolean
isEmptyGlobs { include, exclude } = null include && null exclude

emptyGlobs :: Globs
emptyGlobs = { include: [], exclude: [] }
