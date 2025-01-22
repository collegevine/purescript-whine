module Test.Core.CaseBranchIndentation where

import Test.Prelude

import JSON as JSON
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Whine.Core.CaseBranchIndentation as CaseBranchIndentation
import Whine.Test (runRule)

spec :: Spec Unit
spec = describe "CommaFirstArrays" do

  it "Reports inconsistent indentation" $
    hasViolations ["0:4-3:5"] """
      x = case a of
        b -> c
        d ->
          e
    """

  it "Reports inconsistent guards" $
    hasViolations ["0:4-5:8","7:4-12:5"] """
      x = case a of
        b
          | x ->
              c
          | y -> c
        d -> e

      x = case a of
        b
          | x -> c
          | y -> c
        d ->
          e
    """

  it "Allows consistent indentation" $
    hasViolations [] """
      x = case a of
        b -> c
        d -> e
    """

  it "Allows guards" $
    hasViolations [] """
      x = case a of
        b
          | x -> c
          | y -> c
          | A a <- z -> c
        d -> e
    """

  where
    hasViolations vs mod =
      runRule
        { rule: CaseBranchIndentation.rule JSON.null
        , module: mod
        , assertViolationMessage: (_ `shouldEqual` "Inconsistent indentation in case branches: keep either all single-line or all multi-line")
        }
      >>= shouldEqual vs
