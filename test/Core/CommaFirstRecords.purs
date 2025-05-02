module Test.Core.CommaFirstRecords where

import Test.Prelude

import JSON as JSON
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Whine.Core.CommaFirstRecords as CommaFirstRecords
import Whine.Test (runRule)

spec :: Spec Unit
spec = describe "CommaFirstRecords" do

  it "Reports comma-last array" $
    hasViolations ["0:4-3:1"] """
      x = {
        x: 1,
        y: 2
      }
    """

  it "Reports misaligned commas" $
    hasViolations ["4:4-4:15"] """
      x =
        { x: 1
        , y:
          { a: 5
          ,  b: "foo"
          }
        , z: true
        }
    """

  it "Reports misaligned open brace" $
    hasViolations ["1:2-1:9"] """
      x =
        {  x: 1
        , y: 2
        }
    """

  it "Reports misaligned close brace" $
    hasViolations ["2:8-3:2"] """
      x =
        { x: 1
        , y: 2
       }
    """

  it "Reports closing brace on same line" $
    hasViolations ["2:8-2:10"] """
      x =
        { x: 1
        , y: 2 }
    """

  it "Reports the smallest range that encompasses all violations" $
    hasViolations ["3:2-6:6"] """
      x =
        { x: 1
        , y: 2
        ,  z: 3
        , a: 4
        , b: 5, c: 6,
        e: 7
        , f: 8
        }
    """

  it "Allows comma-first" $
    hasViolations [] """
      x = length
        { x: 1
        , y: 3
        , z: 4
        }
    """

  it "Allows single-line records" $
    hasViolations [] "x = { x: 1, y: 2 }"

  it "Allows multiple elements on one line" $
    hasViolations [] """
      x =
        { x: 1, y: 2
        , z: 3
        , a: 4, b: 5, c: 6
        }
    """

  where
    hasViolations vs mod =
      runRule
        { rule: CommaFirstRecords.rule JSON.null
        , module: mod
        , assertViolationMessage: (_ `shouldEqual` "Format record literals comma-first, align fields vertically")
        }
      >>= shouldEqual vs
