module Whine.Prelude
  ( unionManyRanges
  , module Prelude
  , module Reexport
  , module Whine.Prelude
  )
  where

import Prelude

import Control.Alternative ((<|>), guard) as Reexport
import Control.Monad.Error.Class (try) as Reexport
import Control.Monad.Except (mapExcept, mapExceptT, runExcept, runExceptT) as Reexport
import Control.Monad.Writer (class MonadWriter, WriterT, execWriterT, mapWriterT, tell) as Reexport

import Data.Array ((!!), (:), (..), (\\), concatMap, groupAllBy, find, findMap, foldl, head, nub, null, cons, drop, dropWhile, elem, filter, length, catMaybes, mapMaybe, last, partition, take, uncons, unsnoc, zip, zipWith) as Reexport
import Data.Array.NonEmpty (NonEmptyArray) as Reexport
import Data.Bifunctor (bimap, lmap, rmap) as Reexport
import Data.Either (Either(..), fromRight, either, hush, note) as Reexport
import Data.Foldable (maximum, maximumBy) as Reexport
import Data.FoldableWithIndex (foldlWithIndex, forWithIndex_) as Reexport
import Data.Identity (Identity(..)) as Reexport
import Data.Map (Map) as Reexport
import Data.Maybe (Maybe(..), isNothing, isJust, fromMaybe, maybe) as Reexport
import Data.Newtype (wrap, unwrap, un) as Reexport
import Data.Nullable (Nullable) as Reexport
import Data.Profunctor (dimap) as Reexport
import Data.String (Pattern(..), Replacement(..), joinWith) as Reexport
import Data.String.NonEmpty (NonEmptyString) as Reexport
import Data.Traversable (any, fold, for, for_, intercalate, traverse, traverse_, sequence, sequence_) as Reexport
import Data.Tuple (Tuple(..), fst, snd) as Reexport
import Data.Tuple.Nested (type (/\), (/\)) as Reexport

import Effect (Effect) as Reexport
import Effect.Class (class MonadEffect, liftEffect) as Reexport

import PureScript.CST.Types (SourceRange) as Reexport

import JSON (JSON) as Reexport
import Node.Encoding (Encoding(..)) as Reexport
import Node.Path (FilePath) as Reexport
import Type.Row (type (+)) as Reexport

unionManyRanges :: Array Reexport.SourceRange -> Reexport.Maybe Reexport.SourceRange
unionManyRanges = Reexport.foldl go Reexport.Nothing
  where
    go Reexport.Nothing r = Reexport.Just r
    go (Reexport.Just a) b = Reexport.Just $ unionRanges a b

unionRanges :: Reexport.SourceRange -> Reexport.SourceRange -> Reexport.SourceRange
unionRanges a b = { start: min a.start b.start, end: max a.end b.end }
  where
    lt x y = x.line < y.line || (x.line == y.line && x.column < y.column)
    min x y = if x `lt` y then x else y
    max x y = if x `lt` y then y else x
