-- | Takes a list of modules and their preferred qualifiers, and checks that
-- | those modules are always imported with those qualifiers.
-- |
-- | Configuration should look like this:
-- |
-- |     {
-- |       "Important.Types": ["IT", "ImpTypes"],
-- |       "Prelude": ["P", ""], # empty string means unqualified import is allowed
-- |       ...
-- |     }
-- |
-- | There is no "default" configuration. If this rule is not configured, it
-- | will not emit any warnings.
-- |
module Whine.Core.ModuleQualifiers where

import Whine.Prelude

import Data.Array (notElem)
import Data.Codec.JSON as CJ
import Data.Codec.JSON.Common as CJ.Common
import Data.Codec.JSON.Record as CJR
import Data.Map as Map
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Types (ImportDecl(..), Module(..), ModuleHeader(..), ModuleName(..), Name(..))
import Whine.Types (Rule(..), reportViolation)

rule :: Map ModuleName (Array String) -> Rule
rule mods = Rule \(Module { header: ModuleHeader { imports }}) ->
  for_ imports \imprt@(ImportDecl { qualified, module: Name m }) -> do

    let qualifier = case qualified of
          Just (_ /\ Name { name: ModuleName q }) -> q
          Nothing -> ""
        allowed = Map.lookup m.name mods # fromMaybe []

    when (not null allowed && qualifier `notElem` allowed) $
      reportViolation
      { source: Just $ rangeOf imprt
      , message: fold
          [ "Module ", unwrap m.name
          , " should be imported "
          , printQualifiers allowed
          , ", but here is imported as ", qualifier
          ]
      }

  where
    printQualifiers = unsnoc >>> case _ of
      Nothing -> "" -- should not happen, as we check for not null above
      Just { init: [], last: q } -> printQualifier q
      Just { init: [a], last: b } -> printQualifier a <> " or " <> printQualifier b
      Just { init: qs, last: q } -> intercalate ", " (printQualifier <$> qs) <> ", or " <> printQualifier q

    printQualifier "" = "unqualified"
    printQualifier q = "as " <> q

codec :: CJ.Codec (Map ModuleName (Array String))
codec =
  dimap (overMap unwrap >>> { modules: _ }) (_.modules >>> overMap wrap) $
    CJR.object { modules: CJ.Common.strMap (CJ.array CJ.string) }

  where
    overMap :: ∀ a b. Ord b => (a -> b) -> Map a (Array String) -> Map b (Array String)
    overMap f a = Map.fromFoldable $ lmap f <$> (Map.toUnfoldable a :: Array _)
