module Test.Core.CaseBranchIndentation where

import Test.Prelude

import JSON as JSON
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Whine.Core.CaseBranchIndentation as CaseBranchIndentation
import Whine.Test (runRule)

spec :: Spec Unit
spec = describe "CaseBranchIndentation" do

  it "Reports inconsistent indentation" $
    hasViolations ["1:2-1:8"] """
      x = case a of
        b -> c
        d ->
          e
    """

  it "Reports inconsistent guards" $
    hasViolations ["2:4-3:9","11:2-12:5"] """
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

  it "Reports the smallest range that encompasses all violations" $
    hasViolations ["2:2-8:9"] """
      x = case a of
        b -> c
        d ->
          e
        x -> y
        z
          | a -> b
          | c ->
              d
          | e -> f
    """

  it "Reports either just single-lines or just multilines, whichever are in the minority" $
    hasViolations ["2:2-3:5", "9:2-9:8"] """
      x = case a of
        b -> c
        d ->
          e
        x -> y

      y = case b of
        c ->
          d
        e -> f
        g ->
          h
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
