module Panini.Frontend.Python.Typing.Unify where

import Control.Monad
import Control.Monad.Trans.Except
import Data.IntSet qualified as IntSet
import Panini.Frontend.Python.Typing.Monad
import Panini.Frontend.Python.Typing.PyType
import Prelude

------------------------------------------------------------------------------

-- TODO: we need a proper type hierarchy

unify :: PyType -> PyType -> Infer PyType
unify t1@(MetaVar a) b = do
  when (a `occursIn` b) $ throwE $ InfiniteType t1 b
  t <- maybe (return b) (unify b) =<< lookupMetaVar a
  setMetaVar a t
  return t

unify a (MetaVar b) = unify (MetaVar b) a
unify (Callable as b) (Callable cs d) = Callable <$> zipWithM unify as cs <*> unify b d
unify Any b = return b
unify a Any = return a
unify Bool Bool = return Bool
unify Int Int = return Int
unify Str Str = return Str
unify Int Number = return Int
unify Float Number = return Float
unify Complex Number = return Complex
unify Number Int = return Int
unify Number Float = return Float
unify Number Complex = return Complex
unify Str Sized = return Str
unify Int Slice = return Int
unify a Object = return a
unify (Sequence a) Str = unify a Str >> return Str
unify Str (Sequence a) = unify a Str >> return Str
unify (Sequence a)(Sequence b) = Sequence <$> unify a b

unify (Union as) b = tryUntil $ map (unify b) as
unify a (Union bs) = unify (Union bs) a

unify a b = throwE $ CannotUnify a b

occursIn :: Int -> PyType -> Bool
occursIn m t = IntSet.member m (metaVars t)

