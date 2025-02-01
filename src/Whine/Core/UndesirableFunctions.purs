-- | Takes a list of functions (fully qualified) that "shouldn't" be used in code
-- | (because they're bad/dangerous somehow), with an optional message to e.g.
-- | explain why the function shouldn't be used or propose alternatives.
-- |
-- | Configuration should look like this:
-- |
-- |     {
-- |       "Module.f": "Don't use this function, it's unsafe",
-- |       "Another.Module.g": "Use Third.Module.h instead",
-- |       ...
-- |     }
-- |
-- | There is no "default" configuration. If this rule is not configured, it
-- | will not emit any warnings.
-- |
module Whine.Core.UndesirableFunctions where

import Whine.Prelude

import Data.Codec.JSON as CJ
import Data.Codec.JSON.Common as CJ.Common
import Data.Codec.JSON.Record as CJR
import Data.Map as Map
import Data.String as String
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Types (Expr(..), Ident(..), Import(..), ImportDecl(..), Module(..), ModuleHeader(..), ModuleName, Name(..), Operator(..), QualifiedName(..), Separated(..), Wrapped(..))
import Whine.Types (Handle(..), Rule, currentModule, emptyRule, reportViolation)

rule :: Map { function :: String } (Map (Maybe ModuleName) String) -> Rule
rule badFunctions = emptyRule { onExpr = onExpr }
  where
    onExpr = Handle case _ of

      e@(ExprIdent (QualifiedName { name: Ident function, module: mod }))
        | Just mods <- Map.lookup { function } badFunctions ->
            currentModule \m -> do
              case findImport m mod function of
                Just imprt ->
                  for_ (Map.lookup (Just imprt) mods <|> Map.lookup Nothing mods) \message ->
                    reportViolation { source: Just $ rangeOf e, message }
                Nothing ->
                  case Map.lookup Nothing mods of
                    Just message ->
                      reportViolation { source: Just $ rangeOf e, message }
                    Nothing ->
                      for_ mods \message ->
                        reportViolation { source: Just $ rangeOf e, message }

      _ -> do
        pure unit

    findImport :: ∀ e. Module e -> Maybe ModuleName -> String -> Maybe ModuleName
    findImport (Module { header: ModuleHeader { imports } }) mod ident =
      imports # findMap \(ImportDecl i@{ module: Name { name } }) -> case i of
        { qualified: Just (_ /\ Name { name: qualifier }) } | Just qualifier == mod ->
          Just name
        { names: Just (_ /\ (Wrapped { value: Separated { head, tail } })) } | sameIdentifier head || any (sameIdentifier <<< snd) tail ->
          Just name
        _ ->
          Nothing
      where
        sameIdentifier (ImportValue (Name { name: Ident name })) = name == ident
        sameIdentifier (ImportOp (Name { name: Operator op })) = op == ident
        sameIdentifier _ = false


codec :: CJ.Codec (Map { function :: String } (Map (Maybe ModuleName) String))
codec =
  dimap explodeMaps foldMaps $
    CJR.object { functions: CJ.Common.strMap CJ.string }

  where
    fromQualified :: String -> Maybe ModuleName /\ String
    fromQualified s = case unsnoc $ String.split (Pattern ".") s of
      Just { init, last } | not null init ->
        Just (wrap $ joinWith "." init) /\ last
      _ ->
        Nothing /\ s

    explodeMaps :: Map { function :: String } (Map (Maybe ModuleName) String) -> { functions :: Map String String }
    explodeMaps mm =
      { functions: Map.fromFoldable do
          { function } /\ modules <- (Map.toUnfoldable mm :: Array _)
          mod /\ message <- (Map.toUnfoldable modules :: Array _)
          let modulePrefix = fromMaybe "" $ mod <#> unwrap <#> (_ <> ".")
          pure $ (modulePrefix <> function) /\ message
      }

    foldMaps :: { functions :: Map String String } -> Map { function :: String } (Map (Maybe ModuleName) String)
    foldMaps mm = Map.unions do
      maybeQualifiedFunction /\ message <- (Map.toUnfoldable mm.functions :: Array _)
      let mod /\ function = fromQualified maybeQualifiedFunction
      pure $ Map.singleton { function } $ Map.singleton mod message
