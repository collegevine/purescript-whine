module Whine.Traversals where

import Whine.Prelude

import PureScript.CST.Traversal as T
import PureScript.CST.Types as CST

type Traversal e m =
  { onDecl :: CST.Declaration e -> m Unit
  , onExpr :: CST.Expr e -> m Unit
  , onType :: CST.Type e -> m Unit
  , onBinder :: CST.Binder e -> m Unit
  }

emptyTraversal :: ∀ m e. Monad m => Traversal e m
emptyTraversal =
  { onDecl: \_ -> pure unit
  , onExpr: \_ -> pure unit
  , onType: \_ -> pure unit
  , onBinder: \_ -> pure unit
  }

traverseModule :: ∀ m e. Monad m => Traversal e m -> CST.Module e -> m Unit
traverseModule t = void <<<
  T.rewriteModuleBottomUpM
    { onDecl: \d -> t.onDecl d $> d
    , onExpr: \e -> t.onExpr e $> e
    , onType: \ty -> t.onType ty $> ty
    , onBinder: \b -> t.onBinder b $> b
    }

everywhereOnDecls :: ∀ m e. Monad m => (CST.Declaration e -> m Unit) -> CST.Module e -> m Unit
everywhereOnDecls f = traverseModule emptyTraversal { onDecl = f }

everywhereOnExprs :: ∀ m e. Monad m => (CST.Expr e -> m Unit) -> CST.Module e -> m Unit
everywhereOnExprs f = traverseModule emptyTraversal { onExpr = f }

everywhereOnTypes :: ∀ m e. Monad m => (CST.Type e -> m Unit) -> CST.Module e -> m Unit
everywhereOnTypes f = traverseModule emptyTraversal { onType = f }

separatedToArray :: ∀ a. CST.Separated a -> Array a
separatedToArray (CST.Separated { head, tail }) = head : (snd <$> tail)