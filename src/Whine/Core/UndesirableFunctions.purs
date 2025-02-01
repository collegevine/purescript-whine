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

-- Config for this rule is a two-level map: first level is names of functions
-- that are undesirable, and under each function a list of modules whence this
-- function could be imported. In the YAML config this is defined in a one-level
-- map like this:
--
--     functions:
--       Module1.f: don't use Module1.f
--       Module2.f: don't use Module2.f
--       Another.Module.g: don't use Another.Module.g
--       h: don't use h
--
-- But for efficient lookup at runtime we transform this into a two-level map
-- like this:
--
--     { function: f }:
--        Just Module1: don't use Module1.f
--        Just Module2: don't use Module2.f
--     { function: g }:
--        Just Another.Module: don't use Another.Module.g
--     { function: h }:
--        Nothing: don't use h
--
type Args = Map { function :: String } (Map (Maybe ModuleName) String)

rule :: Args -> Rule
rule badFunctions = emptyRule { onExpr = onExpr }
  where
    onExpr = Handle case _ of

      e@(ExprIdent (QualifiedName { name: Ident function, module: mod }))
        | Just mods <- Map.lookup { function } badFunctions -> -- This function is on the list of undesirables.
            currentModule \m -> do
              let report message = reportViolation { source: Just $ rangeOf e, message }
              case findImport m mod function of
                Just imprt -> do -- Found whence this function is imported.
                  let msg =
                        Map.lookup (Just imprt) mods -- See if we have a message for this specific import.
                        <|> Map.lookup Nothing mods -- If not, see if we have a module-agnostic message for this function.
                  report `traverse_` msg
                Nothing -> -- Couldn't find this function in any imports. It could have been imported via an "open" import.
                  case Map.lookup Nothing mods of
                    Just message -> -- Found a module-agnostic message for this function => report it.
                      report message
                    Nothing -> -- No message found for this function => report all messages just in case.
                      report `traverse_` mods

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


codec :: CJ.Codec Args
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

    explodeMaps :: Args -> { functions :: Map String String }
    explodeMaps mm =
      { functions: Map.fromFoldable do
          { function } /\ modules <- (Map.toUnfoldable mm :: Array _)
          mod /\ message <- (Map.toUnfoldable modules :: Array _)
          let modulePrefix = fromMaybe "" $ mod <#> unwrap <#> (_ <> ".")
          pure $ (modulePrefix <> function) /\ message
      }

    foldMaps :: { functions :: Map String String } -> Args
    foldMaps mm = Map.unions do
      maybeQualifiedFunction /\ message <- (Map.toUnfoldable mm.functions :: Array _)
      let mod /\ function = fromQualified maybeQualifiedFunction
      pure $ Map.singleton { function } $ Map.singleton mod message
