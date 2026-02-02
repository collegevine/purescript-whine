module Test.Core.ModuleQualifiers where

import Test.Prelude

import Data.Map as Map
import PureScript.CST.Types (ModuleName(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Whine.Core.ModuleQualifiers as ModuleQualifiers
import Whine.Test (formatRange, runRule')

spec :: Spec Unit
spec = describe "ModuleQualifiers" do

  it "Reports incorrect qualifiers" $
    hasViolations
      [ "0:0-0:13" /\ "Module A should be imported as A or as AA, but here is imported as Z"
      , "1:0-1:13" /\ "Module B should be imported as B, as BB, or as BBB, but here is imported as Y"
      , "2:0-2:13" /\ "Module C should be imported as C, as CC, as CCC, or unqualified, but here is imported as W"
      , "3:0-3:13" /\ "Module D should be imported unqualified, but here is imported as V"
      , "4:0-4:13" /\ "Module E should be imported as E, but here is imported as U"
      , "5:0-5:13" /\ "Module F should be imported as F or as FF, but here is imported as T"
      ]
      """
        import A as Z
        import B as Y
        import C as W
        import D as V
        import E as U
        import F as T
      """

  it "Allows correct qualifiers" $
    hasViolations []
      """
        import A as A
        import A as AA
        import B as B
        import C
      """

  where
    config = Map.fromFoldable
      [ ModuleName "A" /\ ["A", "AA"]
      , ModuleName "B" /\ ["B", "BB", "BBB"]
      , ModuleName "C" /\ ["C", "CC", "CCC", ""]
      , ModuleName "D" /\ [""]
      , ModuleName "E" /\ ["E"]
      , ModuleName "F" /\ ["F", "FF"]
      ]

    hasViolations vs mod =
      runRule'
        { rule: ModuleQualifiers.rule config
        , module: mod
        }
      <#> map (\v -> fromMaybe "" (formatRange <$> v.source) /\ v.message)
      >>= shouldEqual vs
