module Test.Core.UndesirableModules where

import Test.Prelude

import Data.Map as Map
import PureScript.CST.Types (ModuleName(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Whine.Core.UndesirableModules as UndesirableModules
import Whine.Test (formatRange, runRule')

spec :: Spec Unit
spec = describe "UndesirableModules" do

  it "Reports bad imports" $
    hasViolations
      [ "0:7-0:16" /\ "Do not use BadModule"
      , "2:7-2:21" /\ "Do not use Another.Module"
      , "3:7-3:16" /\ "Do not use BadModule"
      ]
      """
        import BadModule
        import Good.Module
        import Another.Module as AM
        import BadModule (foo) as BM
      """

  where
    config = Map.fromFoldable
      [ ModuleName "BadModule" /\ "Do not use BadModule"
      , ModuleName "Another.Module" /\ "Do not use Another.Module"
      ]

    hasViolations vs mod =
      runRule'
        { rule: UndesirableModules.rule config
        , module: mod
        }
      <#> map (\v -> fromMaybe "" (formatRange <$> v.source) /\ v.message)
      >>= shouldEqual vs
