module Panini.SMT.ReversableCon(ReversableCon(..), ReversedCon(..), reverseConToResult) where


import Panini.Syntax.Predicates
import Panini.Syntax.Primitives
import Panini.Solver.Constraints
import Panini.Syntax.Names
import Data.Text (pack) 
import Data.Set (Set, empty, insert, fromList, union, elems)
import Control.Monad.Trans.State.Strict
import Prelude
import Panini.Syntax

class ReversableCon a where
  -- | Reverse a constraint to eleminate implications.
  --
  -- @reverseCon p => q  === p ∧ ¬ q  Where the smt result needs to be unsat.@
  reverseCon :: a -> SMTContext ReversedCon

type SMTContext = State SMTState

data SMTState = SMTState {
    smtConsts :: Set (Name, PrimitiveBase),
    counter :: Int
}

data ReversedCon
  = RCHead !Pred                    -- p
  | RCAnd !ReversedCon !ReversedCon -- c1 ∧ c2
  | RCNot !ReversedCon              -- ¬c
  | RCOR ![ReversedCon]             -- c1 ∨ c2 ∨ ...


instance ReversableCon Con where
  reverseCon :: Con -> SMTContext ReversedCon
  reverseCon = \case
    CHead p      -> return $ RCHead p
    CAnd c1 c2   -> do
      rc1 <- reverseCon c1
      rc2 <- reverseCon c2
      return $ RCAnd rc1 rc2
    CAll n b p c  -> do
      n' <- getUniqueName n
      let p' = subst (EVar n') n p
      let c' = subst (EVar n') n c
      rc <- reverseCon c'
      modify (\s -> s {smtConsts = insert (n', b) (smtConsts s)})
      return $ RCAnd (RCHead p') (RCNot rc)


instance ReversableCon FlatCon where
  reverseCon :: FlatCon -> SMTContext ReversedCon
  reverseCon = \case
    (FAll xs p q) -> do
      (p', q', xs') <- substAll xs p q []
      modify (\s -> s {smtConsts = smtConsts s `union` fromList xs'})
      return $ RCAnd (RCHead p') (RCNot (RCHead q'))
    where substAll [] p q ys = return (p, q, ys)
          substAll ((n, b):xs) p q ys = do
            n' <- getUniqueName n
            let p' = subst (EVar n') n p
            let c' = subst (EVar n') n q
            substAll xs p' c' ((n', b):ys)

getUniqueName :: Name -> SMTContext Name
getUniqueName (Name t pv) = do
  c <- counter <$> get
  let t' = t <> pack ( "_" ++ show  c)
  modify (\s -> s {counter = counter s + 1})
  return (Name t' pv)

-- | Executes the SMTContext monad and returns the reversed constraints and the new constants.
-- | As the forall implications are eliminated, the State is needed to track the new constants.
--
-- | If the constraints are list, they are mapped to a single or-constraint, as every single constraint needs to be unsat individually.
reverseConToResult :: ReversableCon a => [a] -> ([ReversedCon], [(Name, PrimitiveBase)])
reverseConToResult cs = ([RCOR rc], elems (smtConsts s))
  where
    (rc, s) = runState (mapM reverseCon cs) (SMTState empty 0)