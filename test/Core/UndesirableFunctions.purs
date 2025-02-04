module Test.Core.UndesirableFunctions where

import Test.Prelude

import Codec.JSON.DecodeError as DecodeError
import Control.Monad.Error.Class (throwError)
import Data.Codec.JSON as CJ
import Data.Map as Map
import Effect.Exception (error)
import JSON as JSON
import PureScript.CST.Types (ModuleName(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Whine.Core.UndesirableFunctions as UndesirableFunctions
import Whine.Test (formatRange, runRule')

spec :: Spec Unit
spec = describe "UndesirableFunctions" do

  it "Reports bad functions" $
    hasViolations
      [ "4:5-4:8 Do not use fn1"
      , "5:4-5:7 Do not use fn2 from Bad.Module"
      , "6:4-6:7 Do not use fn3 from Bad.Module"
      ]
      """
        import Some (fn1)
        import Bad.Module (fn2)
        import Bad.Module (fn3)

        x = [fn1]
        y = fn2
        z = fn3
      """

  it "Falls back to unqualified message when there is no matching import" do
    hasViolations ["1:6-1:9 Do not use fn3"] """
      import Some (fn3)
      x = y fn3
    """

  it "Assumes open-imported module if there is only one" do
    hasViolations ["2:6-2:9 Do not use fn3 from Bad.Module"] """
      import Bad.Module
      import Another.Module (fn2)
      x = y fn3
    """

  it "Does not assume open-imported module if there are several of them" do
    hasViolations [] """
      import Bad.Module
      import Another.Module
      x = y fn2
    """

  it "Falls back to unqualified message when the import is open" do
    hasViolations ["1:6-1:9 Do not use fn3"] """
      import Some
      x = y fn3
    """

  it "Does not report when no import and no unqualified message" do
    hasViolations [] """
      import Some
      x = y fn2
    """

  it "Reports when imported with a qualifier" do
    hasViolations
      [ "2:6-2:13 Do not use fn2 from Bad.Module"
      , "3:6-3:13 Do not use fn3 from Bad.Module"
      ]
      """
        import Bad.Module as FN2
        import Bad.Module (fn3) as FN3
        x = y FN2.fn2
        y = z FN3.fn3
      """

  it "Picks correct message according to the import" do
    hasViolations
      [ "2:6-2:9 Do not use fn2 from Bad.Module"
      , "3:6-3:12 Do not use fn2 from Another.Module"
      ]
      """
        import Bad.Module (fn2)
        import Another.Module as AM
        x = y fn2
        y = z AM.fn2
      """

  it "Allows imports from other modules" do
    hasViolations [] """
      import Innocent.Module (fn2)
      x = y fn2
    """

    hasViolations [] """
      import Innocent.Module as IM
      x = y IM.fn2
    """

  describe "Config parsing" do
    it "parses correctly" do
      json <- unsafeRight $ JSON.parse """
        {
          "functions": {
            "fn1": "Do not use fn1",
            "Bad.fn1": "Do not use fn1 from Bad",
            "Bad.Module.fn1": "Do not use fn1 from Bad.Module",
            "Bad.fn2": "Do not use fn2 from Bad",
            "Bad.Module.fn2": "Do not use fn2 from Bad.Module"
          }
        }
      """

      parsed <- unsafeRight $ lmap DecodeError.print $
        CJ.decode UndesirableFunctions.codec json
        <#> Map.toUnfoldable
        <#> map \({ function } /\ m) ->
                  function /\ (Map.toUnfoldable m <#> \(mn /\ msg) -> (unwrap <$> mn) /\ msg)

      parsed `shouldEqual`
        [ "fn1" /\
            [ Nothing /\ "Do not use fn1"
            , Just "Bad" /\ "Do not use fn1 from Bad"
            , Just "Bad.Module" /\ "Do not use fn1 from Bad.Module"
            ]
        , "fn2" /\
            [ Just "Bad" /\ "Do not use fn2 from Bad"
            , Just "Bad.Module" /\ "Do not use fn2 from Bad.Module"
            ]
        ]
  where
    unsafeRight :: ∀ a. Either String a -> _ a
    unsafeRight = case _ of
      Left e -> throwError $ error $ "Failed to parse config: " <> e
      Right a -> pure a

    config = Map.fromFoldable
      [ { function: "fn1" } /\ Map.singleton
          Nothing "Do not use fn1"
      , { function: "fn2" } /\ Map.fromFoldable
          [ Just (ModuleName "Bad.Module") /\ "Do not use fn2 from Bad.Module"
          , Just (ModuleName "Another.Module") /\ "Do not use fn2 from Another.Module"
          ]
      , { function: "fn3" } /\ Map.fromFoldable
          [ Nothing /\ "Do not use fn3"
          , Just (ModuleName "Bad.Module") /\ "Do not use fn3 from Bad.Module"
          ]
      ]

    hasViolations vs mod =
      runRule'
        { rule: UndesirableFunctions.rule config
        , module: mod
        }
      <#> map (\v -> fromMaybe "" (formatRange <$> v.source) <> " " <> v.message)
      >>= shouldEqual vs
