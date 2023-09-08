{-# LANGUAGE OverloadedLists #-}
module Panini.Solver.Grammar 
  ( GCon(..)
  , grammarConstraints
  , gconKVar
  , solve
  , solveAll  
  ) where

import Algebra.Lattice
import Control.Monad.Extra
import Data.Either (partitionEithers)
import Data.Foldable
import Data.Function
import Data.Generics.Uniplate.Operations qualified as Uniplate
import Data.Hashable
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.List (partition,tails)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe
import GHC.Generics
import Panini.Abstract.AExpr
import Panini.Abstract.AValue
import Panini.Abstract.Semantics
import Panini.Error
import Panini.Monad
import Panini.Panic
import Panini.Pretty
import Panini.Provenance
import Panini.Solver.Assignment
import Panini.Solver.Constraints
import Panini.Syntax
import Prelude

-------------------------------------------------------------------------------

-- TODO: could this go into Constraints.hs ?

-- | A /grammar constraint/ is any constraint of the form @∀s:𝕊. κ(s) ⇒ c@.
-- Here, @κ@ is known as a /grammar variable/ and @c@ as a /grammar consequent/.
-- The string variable @s@, applied parameter to @κ@, is a free variable in @c@.
data GCon = GCon Name KVar Con
  deriving stock (Eq, Show, Read, Generic)

instance Hashable GCon

instance Pretty GCon where
  pretty (GCon x k c) = pretty $ CAll x TString (PAppK k [EVar x]) c

-- | Returns all grammar constraints within the given constraint.
grammarConstraints :: Con -> HashSet GCon
grammarConstraints c0 = HashSet.fromList
  [GCon x k c | CAll x TString (PAppK k [EVar y]) c <- Uniplate.universe c0
              , y == x
  ]

gconKVar :: GCon -> KVar
gconKVar (GCon _ k _) = k

-------------------------------------------------------------------------------

-- | Solve a set of grammar constraints, returning the combined solution.
-- 
-- The current approach orders constraints by κ variable name and tries to solve
-- them sequentially, applying intermediate solutions on the way.
solveAll :: HashSet GCon -> Pan Assignment
solveAll = foldM solve1 mempty
         . List.sortBy (compare `on` gconKVar)
         . HashSet.toList
  where
    solve1 s (GCon x k c) = do
      logMessage $ "Solve grammar variable" <+> pretty k
      g <- solve $ GCon x k $ apply s c
      logMessage $ "Found grammar assignment for" <+> pretty k
      logData g
      return $ Map.union g s

-- | Solve a grammar constraint @∀s:𝕊. κ(s) ⇒ c@, returning a solution for @κ@.
solve :: GCon -> Pan Assignment

-- Trick to solve (simple) recursions: eliminate recursive κ applications by
-- replacing them simply with true, then try to solve normally. If the
-- recursions did not actually affect the grammar, the solution will validate
-- the VC, which still includes the recursive applications. Otherwise, the
-- recursion must have encoded some information that was lost by this simple
-- elimination and the VC will be judged invalid.
solve (GCon s k c) | k `elem` kvars c = do
  logData c
  logMessage $ "Eliminate recursive grammar variable" <+> pretty k
  let c' = apply [(k,PTrue)] c
  solve (GCon s k c')

-- TODO: clean up
solve (GCon s k c) = do
  logData c
  c' <- rewrite c
  logMessage "abstract string from rewritten constraints"
  g <- joins <$> (forM (unDNF c') $ \ps -> do
    logData ps
    gs <- meets <$> (forM ps $ \p -> do
      logMessage $ "abstract s from" <+> pretty p
      g <- abstractVarString s p
      logData g
      return g)
    logData gs
    return gs)
  p <- PRel <$> concretizeVar s (EAbs $ AString g)

  -- IMPORTANT: we need to substitute the free string variable s in the
  -- grammar solution with the generic κ parameter, so that later on we can
  -- apply without problems
  let p' = subst (EVar $ head $ kparams k) s p

  return $ Map.singleton k p'

abstractVarString :: Name -> Rel -> Pan AString
abstractVarString x r = abstractVar x TString r >>= \case
  EAbs (AString s) -> return s
  a -> panic $ "expected abstract string instead of" <+> pretty a

-------------------------------------------------------------------------------

rewrite :: Con -> Pan (DNF Rel)
rewrite = \case
  CHead p      -> return $ toDNF p
  CAnd c1 c2   -> liftA2 (∧) (rewrite c1) (rewrite c2)
  CAll x b p c -> case c of
    CAll x2 b2 p2 c2 -> varElimDNF x b =<< rewrite (CAll x2 b2 (p ∧ p2) c2)
    CHead q          -> varElimDNF x b $ toDNF $ p ∧ q
    CAnd c1 c2 -> (joins <$>) . forM (unDNF $ toDNF p) $ \p' -> do
        c1' <- varElimDNF x b =<< meet (DNF [p']) <$> rewrite c1
        c2' <- varElimDNF x b =<< meet (DNF [p']) <$> rewrite c2
        return $ c1' ⟑ c2'

(⟑) :: DNF a -> DNF a -> DNF a
DNF [] ⟑ DNF [] = DNF []
p      ⟑ DNF [] = p
DNF [] ⟑ q      = q
p      ⟑ q      = p ∧ q

-------------------------------------------------------------------------------

newtype DNF a = DNF { unDNF :: [[a]] }

instance MeetSemilattice (DNF a) where
  DNF ps ∧ DNF qs = DNF [p ++ q | p <- ps, q <- qs]

instance BoundedMeetSemilattice (DNF a) where
  top = DNF [[]]

instance JoinSemilattice (DNF a) where
  DNF [[]] ∨ _        = DNF [[]]
  _        ∨ DNF [[]] = DNF [[]]
  DNF ps   ∨ DNF qs   = DNF (ps ++ qs)

instance BoundedJoinSemilattice (DNF a) where
  bot = DNF []

-- TODO: clean up
toDNF :: Pred -> DNF Rel
toDNF p0 = DNF $ unwrapDNF $ flip Uniplate.rewrite p0 $ \case
  PAnd xs
    | PFalse `elem` xs -> Just PFalse
    | PTrue `elem` xs -> Just $ PAnd $ List.filter (/= PTrue) xs
    | any isPAnd xs ->
      let (ys, zs) = partition isPAnd xs
      in Just $ PAnd $ mconcat [y | PAnd y <- ys] ++ zs
    | any isPOr xs -> case partition isPOr xs of
        (POr ys : yys, zs) -> Just $ POr $ [PAnd $ y : (yys ++ zs) | y <- ys]
        _                  -> impossible
    | or [PRel (inverse r) `elem` xs | PRel r <- xs] -> Just PFalse
    | HashSet.size (HashSet.fromList xs) < length xs -> Just $ PAnd $ List.nub xs  -- TODO

  POr xs
    | PTrue `elem` xs -> Just PTrue
    | PFalse `elem` xs -> Just $ POr $ List.filter (/= PFalse) xs
    | any isPOr xs ->
      let (ys, zs) = partition isPOr xs
      in Just $ POr $ mconcat [y | POr y <- ys] ++ zs

  PNot (POr xs) -> Just $ PAnd $ map PNot xs
  PNot (PAnd xs) -> Just $ POr $ map PNot xs
  PNot (PRel r) -> Just $ PRel $ inverse r

  PImpl a b -> Just $ POr [PNot a, b]
  PIff a b -> Just $ POr [PAnd [a, b], PAnd [PNot a, PNot b]]

  _ -> Nothing

unwrapDNF :: Pred -> [[Rel]]
unwrapDNF = \case
  POr  xs -> unOr xs
  PAnd ys -> [unAnd ys]
  PTrue   -> [[]]
  PFalse  -> []
  PRel r  -> [[r]]
  p -> panic $ "expected POr/PAnd/PTrue/PFalse/PRel instead of" <+> pretty p
  where
    unAnd ys
      | all isPRel ys = [y | PRel y <- ys]
      | otherwise = panic $ "expected all PRel instead of" <+> pretty ys
    unOr xs
      | all isPAnd xs = [unAnd ys | PAnd ys <- xs]
      | otherwise = panic $ "expected all PAnd instead of" <+> pretty xs

-------------------------------------------------------------------------------

varElimDNF :: Name -> Base -> DNF Rel -> Pan (DNF Rel)
varElimDNF x b ps = DNF <$> mapMaybeM (varElim x b) (unDNF ps)

-- TODO: clean up
-- TODO: update submission
-- | Algorithm 3 in OOPSLA'23 submission.
varElim :: Name -> Base -> [Rel] -> Pan (Maybe [Rel])
varElim _ TUnit ps = return $ Just ps  -- TODO
varElim x b ps = do
  logMessage $ "varElim" <+> pretty x <+> pretty b
  logData $  ps
  let (pxs, ps') = List.partition (elem x . freeVars) ps
  logData (pxs, ps')
  let (pxs', xvs) = partitionEithers $ map (maybeToEither (isolateVar x)) pxs
  logData (xvs, pxs')

  let bTop = topExpr b

  let x̂s₀ = Map.singleton [] bTop
  let refine x̂s xv = do
        logMessage $ "refine" <+> pretty xv
        let vs = freeVars xv
        let x̂ = fromMaybe bTop $ Map.lookup vs x̂s
        case x̂ ∧? xv of
          Just x̂' -> return $ Map.insert vs x̂' x̂s
          Nothing -> throwError $ MeetImpossible x̂ xv NoPV  
  x̂s <- foldM refine x̂s₀ xvs
  logData x̂s

  if any containsBotAExpr x̂s then do
    logMessage "Nothing"
    return Nothing
  else do
    let x̂s1 = map snd $ Map.toAscList x̂s
    let qs1 = List.nub $ map normRel $ [x̂₁ :=: x̂₂ | (x̂₁:rest) <- tails x̂s1, x̂₂ <- rest]
    logData qs1

    let qs2 = concatMap (\px' -> map (\x̂ -> subst x̂ x px') x̂s1) pxs'
    logData qs2

    let qs = qs1 ++ qs2 ++ ps'
    logData qs

    return $ Just qs

maybeToEither :: (a -> Maybe b) -> (a -> Either a b)
maybeToEither f = \x -> maybe (Left x) Right (f x)

containsBotAExpr :: AExpr -> Bool
containsBotAExpr e = or [containsBot a | EAbs a <- Uniplate.universe e]

-------------------------------------------------------------------------------

isPOr :: Pred -> Bool
isPOr (POr _) = True
isPOr _       = False

isPAnd :: Pred -> Bool
isPAnd (PAnd _) = True
isPAnd _        = False

isPRel :: Pred -> Bool
isPRel (PRel _) = True
isPRel _        = False
