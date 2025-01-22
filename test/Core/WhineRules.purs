module Test.Core.WhineRules where

import Test.Prelude

import Test.Core.CaseBranchIndentation as CaseBranchIndentation
import Test.Core.CommaFirstArrays as CommaFirstArrays
import Test.Spec (Spec, describe)

spec :: Spec Unit
spec = describe "Core.WhineRules" do
  CommaFirstArrays.spec
  CaseBranchIndentation.spec
