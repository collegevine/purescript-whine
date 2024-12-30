module Whine where

import Whine.Prelude

import Data.Array.NonEmpty as NEA
import Data.Map as Map
import Data.String as String
import Effect.Exception as Err
import Node.FS.Sync (readTextFile)
import PureScript.CST (RecoveredParserResult(..), parseModule)
import PureScript.CST.Range (class RangeOf)
import PureScript.CST.Traversal (traverseBinder, traverseDecl, traverseExpr, traverseModule, traverseType)
import PureScript.CST.Traversal as T
import PureScript.CST.Types (Expr(..), Module(..), ModuleHeader(..), Separated(..), Wrapped(..))
import PureScript.CST.Types as CST
import Record (merge)
import Whine.Muting (MutedRange(..), mutedRanges)
import Whine.Runner.Glob as Glob
import Whine.Types (Handle(..), RuleSet, Violations, WithFile, WithMuted, WithRule, mapViolation)

-- | Given a parsed PS module, runs all the rules on it. The rules report
-- | violations via Writer side-effects.
runRules :: ∀ m e. Monad m => RangeOf e => RuleSet (WriterT (Violations ()) m) -> Module e -> WriterT (Violations (WithRule + ())) m Unit
runRules rs mdl = void do
  onModule mdl
  traverseModule visitor mdl
  where
    visitor =
      { onBinder: \x -> allRules _.onBinder x *> traverseBinder visitor x
      , onExpr: \x -> allRules _.onExpr x *> traverseExpr' visitor x
      , onDecl: \x -> allRules _.onDecl x *> traverseDecl visitor x
      , onType: \x -> allRules _.onType x *> traverseType visitor x
      }

    allRules :: ∀ x. (_ -> Handle x _) -> x e -> _
    allRules f x =
      forWithIndex_ rs \rid { rule } ->
        let (Handle h) = f rule
        in h x # mapViolation (merge { rule: rid })

    onModule :: Module e -> _
    onModule m@(Module { header: ModuleHeader header }) = do
      allRules _.onModule m
      for_ header.imports \imp ->
        allRules _.onModuleImport imp
      for_ header.exports \(Wrapped { value: Separated { head, tail } }) -> do
        allRules _.onModuleExport head
        for_ tail \(_ /\ exp) -> allRules _.onModuleExport exp


    -------------------------------------------------------------------------------------------------
    --- These two functions are a workaround for this issue:
    ---   https://github.com/natefaubion/purescript-language-cst-parser/pull/59
    -------------------------------------------------------------------------------------------------
    traverseExpr'
      :: forall ee f r
      . Applicative f
      => { onBinder :: T.Rewrite ee f CST.Binder, onExpr :: T.Rewrite ee f CST.Expr, onType :: T.Rewrite ee f CST.Type | r }
      -> T.Rewrite ee f CST.Expr
    traverseExpr' k = case _ of
      CST.ExprApp f args -> ExprApp <$> k.onExpr f <*> traverse (traverseSpine k) args
      anotherExpr -> traverseExpr k anotherExpr

    traverseSpine
      :: forall ee f r
      . Applicative f
      => { onBinder :: T.Rewrite ee f CST.Binder, onExpr :: T.Rewrite ee f CST.Expr, onType :: T.Rewrite ee f CST.Type | r }
      -> T.Rewrite ee f (CST.AppSpine CST.Expr)
    traverseSpine k = case _ of
      CST.AppType tok ty -> CST.AppType tok <$> k.onType ty
      CST.AppTerm expr -> CST.AppTerm <$> k.onExpr expr
    -------------------------------------------------------------------------------------------------
    -------------------------------------------------------------------------------------------------


-- | Given a file path, reads the file, then passes it to `checkModule` (see
-- | comments there).
checkFile :: forall m. MonadEffect m => RuleSet (WriterT (Violations ()) m) -> FilePath -> WriterT (Violations (WithRule + WithMuted + WithFile + ())) m Unit
checkFile rules path = do
  eText <- liftEffect $ try $ readTextFile UTF8 path
  case eText of
    Left err ->
      tell
        [ { message: "Failed to read the file: " <> Err.message err
          , source: Nothing
          , muted: false
          , rule: ""
          , file: { path, lines: Nothing }
          }
        ]
    Right text ->
      checkModule rules { path, text }

-- | Given full text of a PureScript module, parses it and runs all the rules on
-- | it, then amends the rules with the `muted :: Boolean` flag based on the
-- | muting directives in the file itself as well as include/exclude globs in
-- | each rule's config. If reading the file or parsing it fails, those
-- | conditions are reported as linter violations, rather than a big loud crash.
checkModule :: forall m. MonadEffect m =>
  RuleSet (WriterT (Violations ()) m)
  -> { path :: FilePath, text :: String }
  -> WriterT (Violations (WithRule + WithMuted + WithFile + ())) m Unit
checkModule rules { path, text } =
  mapViolation addMutedAndFile
    case parseModule text of
      ParseFailed _ ->
        tell [{ rule: "", source: Nothing, message: "Failed to parse the file" }]
      ParseSucceededWithErrors m _ ->
        runRules rules m
      ParseSucceeded m ->
        runRules rules m
  where
    addMutedAndFile v = v # merge
      { file
      , muted: isMutedByDirective v || isExcludedByPath v
      }

    lines = String.split (Pattern "\n") text
    file = { path, lines: Just lines }
    ranges = mutedRanges { lines }

    isIntersecting src = case _ of
      MutedLine l -> src.start.line == l
      MutedRange { start, end } -> src.start.line >= start && src.end.line <= end

    isMutedByDirective v = fromMaybe false do
      source <- v.source
      rs <- Map.lookup v.rule ranges
      guard $ any (isIntersecting source) rs
      pure true

    isExcludedByPath v =
      let
        rule = Map.lookup v.rule rules
        included = rule >>= _.globs.include # maybe true \i -> Glob.test (NEA.toArray i) path
        excluded = rule >>= _.globs.exclude # maybe false \e -> Glob.test (NEA.toArray e) path
      in
        excluded || not included