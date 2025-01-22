module Test.Core.CommaFirstArrays where

import Test.Prelude

import JSON as JSON
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Whine.Core.CommaFirstArrays as CommaFirstArrays
import Whine.Test (runRule)

spec :: Spec Unit
spec = describe "CommaFirstArrays" do

  it "Reports comma-last array" $
    hasViolations ["0:4-3:1"] """
      x = [
        1,
        2
      ]
    """

  it "Reports misaligned commas" $
    hasViolations ["2:4-4:5"] """
      x =
        [ []
        , [ 1
          ,  2
          ]
        , "more stuff"
        ]
    """

  it "Reports misaligned open bracket" $
    hasViolations ["1:2-3:3"] """
      x =
        [  1
        , 2
        ]
    """

  it "Reports closing bracket on same line" $
    hasViolations ["1:2-2:7"] """
      x =
        [ 1
        , 2 ]
    """

  it "Allows comma-first" $
    hasViolations [] """
      x = length
        [ 1
        , 3
        , 4
        ]
    """

  it "Allows single-line arrays" $
    hasViolations [] "x = [1, 2]"

  it "Allows multiple elements on one line" $
    hasViolations [] """
      x =
        [ 1, 2
        , 3
        , 4, 5, 6
        ]
    """

  where
    hasViolations vs mod =
      runRule
        { rule: CommaFirstArrays.rule JSON.null
        , module: mod
        , assertViolationMessage: (_ `shouldEqual` "Prefer comma-first style in array literals, items aligned vertically")
        }
      >>= shouldEqual vs
