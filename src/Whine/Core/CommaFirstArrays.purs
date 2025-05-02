-- | Checks that array literals are written comma-first style, with elements
-- | aligned vertically, one space to the right of the commas.
-- |
-- | Single-line arrays are ok.
-- |
-- | Several elements on the same line are ok, as long as the first one of them
-- | is aligned correctly.
-- |
-- |     -- Good:
-- |     [ 1
-- |     , 2
-- |     , 3
-- |     ]
-- |
-- |     -- Good:
-- |     [ 1, 2, 3 ]
-- |
-- |     -- Good:
-- |     [ 1, 2
-- |     , 3
-- |     , 4, 5
-- |     , 6
-- |     ]
-- |
-- |     -- Bad:
-- |     [ 1
-- |     ,  2
-- |     , 3
-- |     ]
-- |
-- |     -- Bad:
-- |     [ 1
-- |      , 2
-- |     , 3
-- |     ]
-- |
-- |     -- Bad:
-- |     [
-- |       1,
-- |       2,
-- |       3
-- |     ]
-- |
-- |     -- Bad:
-- |     [ 1, 2,
-- |      3, 4 ]
-- |
module Whine.Core.CommaFirstArrays where

import Whine.Prelude

import PureScript.CST.Range (rangeOf)
import PureScript.CST.Types (Expr(..))
import Whine.Core.CommaFirst (commaFirstRule)
import Whine.Types (Handle(..), Rule, emptyRule)

rule :: JSON -> Rule
rule _ = emptyRule { onExpr = onExpr }
  where
    onExpr :: Handle Expr
    onExpr = Handle case _ of
      ExprArray a ->
        commaFirstRule a rangeOf "Format array literals comma-first, align elements vertically"
      _ ->
        pure unit
