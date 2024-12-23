module Whine.Types where

import Whine.Prelude

import Codec.JSON.DecodeError as DecodeError
import Data.Codec.JSON as CJ
import JSON as JSON
import PureScript.CST.Range (class RangeOf)
import PureScript.CST.Types as CST

-- | This function is the core functionality of a linting rule: it takes an AST
-- | node and reports anything wrong with it as effects in the monad.
-- |
-- | The type variable `e` is a peculiarity of how the `language-cst-parser`
-- | library is structured: the whole AST is parametrized by this type, which
-- | represents possible errors with parsing. When the tree is fully parsed
-- | successfully, `e` is set to `Void`. Otherwise it's some error type.
-- |
-- | Because for the purposes of this library we don't actually care whether the
-- | parsing was successful, the handler function is polymorphic in `e`. But we
-- | have to have a `RangeOf` instance for it, because it's a prerequisite for
-- | other `RangeOf` instances for AST nodes, and we do need those, because we
-- | need to report a source location for every violation.
newtype Handle astNode m = Handle (∀ e. RangeOf e => astNode e -> m Unit)

type RuleId = String

-- | A linting rule is just a collection of `Handle` functions for different
-- | types of AST nodes. See comments on `Handle` for explanation of how it
-- | works.
-- |
-- | When constructing a rule, use the `emptyRule` value as base, so you don't
-- | have to provide every field.
type Rule m =
  { onModule :: Handle CST.Module m
  , onModuleImport :: Handle CST.ImportDecl m
  , onModuleExport :: Handle CST.Export m
  , onDecl :: Handle CST.Declaration m
  , onBinder :: Handle CST.Binder m
  , onExpr :: Handle CST.Expr m
  , onType :: Handle CST.Type m
  }

-- | `Violation` is an open record. When generated by a rule implementation, it
-- | starts out with just `source` and `message`, but at higher stack levels
-- | gets annotated with ID of the rule that created the violation (see
-- | `WithRule`), with the file whence the violation came (see `WithFile`), and
-- | with a flag showing if this particular violation was muted (see
-- | `WithMuted`).
type Violation r =
  { source :: Maybe CST.SourceRange
  , message :: String
  | r
  }

type Violations r = Array (Violation r)

-- | See comments on `Violation`
type WithRule r = (rule :: RuleId | r)

-- | See comments on `Violation`
type WithMuted r = (muted :: Boolean | r)

-- | See comments on `Violation`
type WithFile r = (file :: File | r)

-- | See comments on `ruleFactory`
type RuleFactory m = JSON -> Either String (Rule m)

-- | See comments on `ruleFactory`
type RuleFactories m = Array (RuleId /\ RuleFactory m)

-- | For every rule ID we have a rule implementation `Rule m` and some common
-- | config values that can be applied to any rule and are handled by the
-- | framework.
type RuleSet m = Map RuleId
  { rule :: Rule m
  , globs ::
      { include :: Maybe (NonEmptyArray String)
      -- ^ `Nothing` means all files are included. `Just` means only listed files are.

      , exclude :: Maybe (NonEmptyArray String)
      -- ^ `Nothing` means no files are excluded. `Just` means listed files are excluded.
      }
  }

type File = { path :: FilePath, lines :: Maybe (Array String) }

-- | The monad in which each individual rule is run. The `Writer` instance
-- | allows the rule to report violations as it goes.
class MonadWriter (Array (Violation ())) m <= MonadRules m
instance MonadWriter (Array (Violation ())) m => MonadRules m

-- | Surprisingly, I couldn't found a version of `mapWriterT` that just takes a
-- | function `w -> w`
mapViolation :: ∀ a b r m. Monad m => (a -> b) -> WriterT (Array a) m r -> WriterT (Array b) m r
mapViolation f = mapWriterT \x -> do
  r /\ violations <- x
  pure $ r /\ (f <$> violations)

-- | This function is intended to pair rules with their IDs. Every rule would
-- | take its own `args` and provide a JSON codec to decode the args, and the
-- | list of possible rules and their IDs would be constructed like this:
-- |
-- |     factories :: ∀ m. RuleFactories m
-- |     factories =
-- |       [ ruleFactory "RuleOne" ruleOneArgsCodec ruleOne
-- |       , ruleFactory "RuleTwo" ruleTwoArgsCodec ruleTwo
-- |       ]
-- |
-- |     ruleOne :: { arg :: Int, anotherArg :: String } -> Rule m
-- |     ruleTwo :: {} -> Rule m
-- |
-- | `ruleFactory` takes care of parsing the arguments and reporting any errors,
-- | so that the actual rule implementation only has to deal with strongly typed
-- | arguments.
ruleFactory :: ∀ args m. Monad m => RuleId -> CJ.Codec args -> (args -> Rule m) -> RuleId /\ RuleFactory m
ruleFactory rid argsCodec construct =
  rid /\ \args ->
    case construct <$> CJ.decode argsCodec args of
      -- If args == null and the codec rejected it, it means the rule can't work
      -- without config, so we assume it was meant to be disabled.
      Left _ | JSON.isNull args -> Right emptyRule
      Left err -> Left $ DecodeError.print err
      Right rule -> Right rule

emptyRule :: ∀ m. Monad m => Rule m
emptyRule =
  { onModule: Handle \_ -> pure unit
  , onModuleImport: Handle \_ -> pure unit
  , onModuleExport: Handle \_ -> pure unit
  , onDecl: Handle \_ -> pure unit
  , onBinder: Handle \_ -> pure unit
  , onExpr: Handle \_ -> pure unit
  , onType: Handle \_ -> pure unit
  }
