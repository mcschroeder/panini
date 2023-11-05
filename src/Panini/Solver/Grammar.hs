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
import Data.Foldable
import Data.Function
import Data.Generics.Uniplate.Operations qualified as Uniplate
import Data.Hashable
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.List (tails)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe
import GHC.Generics
import Panini.Abstract.AExpr
import Panini.Abstract.AValue
import Panini.Abstract.Semantics
import Panini.Monad
import Panini.Panic
import Panini.Pretty
import Panini.Solver.Assignment
import Panini.Solver.Constraints
import Panini.Solver.Simplifier
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
-- them sequentially, applying intermediate solutions on the way. Multiple
-- solutions to the same variable (i.e., if some κᵢ appears multiple times) are
-- meet-ed together.
solveAll :: HashSet GCon -> Pan Assignment
solveAll = foldM solve1 mempty
         . List.sortBy (compare `on` gconKVar)
         . HashSet.toList
  where
    solve1 s (GCon x k c) = do
      logMessage $ "Solve for grammar variable" <+> pretty k
      g <- solve $ GCon x k $ apply s c
      logMessage $ "Found grammar assignment" <+> pretty g
      return $ Map.unionWith meet' g s
    
    -- TODO: refactor solve to return AString and concretize higher up
    meet' (PRel (s1 :∈: g1)) (PRel (s2 :∈: g2)) 
      | s1 == s2, Just g <- g1 ∧? g2 = PRel (s1 :∈: g)
    meet' p q = p ∧ q

-- | Solve a grammar constraint @∀s:𝕊. κ(s) ⇒ c@, returning a solution for @κ@.
solve :: GCon -> Pan Assignment

-- Trick to solve (simple) recursions: eliminate recursive κ applications by
-- replacing them simply with true, then try to solve normally. If the
-- recursions did not actually affect the grammar, the solution will validate
-- the VC, which still includes the recursive applications. Otherwise, the
-- recursion must have encoded some information that was lost by this simple
-- elimination and the VC will be judged invalid.
-- solve (GCon s k c) | k `elem` kvars c = do
--   logData c
--   logMessage $ "Eliminate recursive grammar variable" <+> pretty k
--   let c' = apply [(k,PTrue)] c
--   solve (GCon s k c')
solve (GCon s k c) | not $ null $ kvars c = do
  logData c
  logMessage $ "Assume nested" <+> kappa <+> "variables to be" <+> pretty PTrue
  let c2 = apply (Map.fromList [(k2, PTrue) | k2 <- toList (kvars c)]) c
  logMessage "Simplify"
  let c3 = simplify c2
  solve (GCon s k c3)

-- TODO: clean up
solve (GCon s k c) = do
  logData c
  logMessage "Rewrite grammar constraint"  
  c1 <- rewrite c
  logData c1
  let c2 = PNot c1
  logData c2
  let c3 = dnf c2
  logData c3
  g0 <- forM c3 $ \rs -> do
    logMessage $ "rs =" <+> pretty rs
    g <- meets <$> mapM (abstractVarString s) rs
    logData g
    return g
  --g <- joins <$> mapM (fmap meets . mapM (abstractVarString s)) c'
  let g = joins g0
  logData g
  let g2 = neg g
  logData g2
  p <- PRel <$> concretizeVar s (EAbs $ AString g2)

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

rewrite :: Con -> Pan Pred
rewrite c0 = do
  --logMessage $ "Eliminate ∀"
  let c1 = elimAll c0
  logData $ group $ "elimAll" <\> pretty c0 <\> "⇝" <\> pretty c1
  --logMessage $ "Eliminate ∃"
  c2 <- elimExists c1
  logData $ group $ "elimExists" <\> pretty c1 <\> "⇝" <\> pretty c2
  -- let c3 = dnf c2
  -- logData $ group $ "dnf" <\> pretty c2 <\> "⇝" <\> pretty c3
  -- return c3
  return c2
 where
  elimAll :: Con -> Pred
  elimAll = \case
    CHead p      -> p
    CAnd c1 c2   -> elimAll c1 ∧ elimAll c2
    CAll x t p c -> PNot $ PExists x t $ PNot $ PImpl p $ elimAll c

  elimExists :: Pred -> Pan Pred
  elimExists = \case
    PTrue         -> return PTrue
    PFalse        -> return PFalse
    PRel r        -> return $ PRel r
    PNot p        -> PNot  <$> elimExists p
    PImpl a b     -> PImpl <$> elimExists a <*> elimExists b
    PIff a b      -> PIff  <$> elimExists a <*> elimExists b
    PAnd xs       -> PAnd  <$> mapM elimExists xs
    POr xs        -> POr   <$> mapM elimExists xs
    PExists x t p -> fromDNF <$> (mapMaybeM (varElim x t) =<< dnf <$> elimExists p)
    PAppK _ _     -> impossible
  
dnf :: Pred -> [[Rel]]
dnf = \case
  PTrue                -> [[]]
  PFalse               -> []
  PRel r               -> [[r]]
  PNot PTrue           -> []
  PNot PFalse          -> [[]]
  PNot (PRel r)        -> [[inverse r]]
  PNot (PNot x)        -> dnf x
  PNot (PAnd xs)       -> dnf $ POr (map PNot xs)
  PNot (POr xs)        -> dnf $ PAnd (map PNot xs)    
  PNot (PImpl a b)     -> dnf $ PAnd [a, PNot b]    
  PNot (PIff a b)      -> dnf $ PIff a (PNot b)  -- TODO: optimize arbitrary choice?
  PNot (PExists _ _ _) -> impossible
  PNot (PAppK _ _)     -> impossible
  PImpl a b            -> dnf $ POr [PNot a, b]
  PIff a b             -> dnf $ POr [PAnd [a,b], PAnd [PNot a, PNot b]]    
  PAnd xs              -> map concat $ sequence $ nub' $ map dnf xs
  POr xs               -> nub' $ concat $ map dnf xs
  PExists _ _ _        -> impossible
  PAppK _ _            -> impossible
  
fromDNF :: [[Rel]] -> Pred
fromDNF [[]] = PTrue
fromDNF []   = PFalse
fromDNF xs   = POr $ map (PAnd . map PRel) xs

nub' :: Hashable a => [a] -> [a]
nub' = HashSet.toList . HashSet.fromList
{-# INLINE nub' #-}


-------------------------------------------------------------------------------

-- TODO: proper normalization
norm' :: Rel -> Maybe Rel
norm' r0 = case normRel r0 of
  e1 :=: e2     
    | e1 == e2 -> Nothing
    | null (freeVars e1), not (null (freeVars e2)) -> norm' (e2 :=: e1)
    | otherwise -> Just (e1 :=: e2)
  r1 -> Just r1

varElim :: Name -> Base -> [Rel] -> Pan (Maybe [Rel])
varElim x b φ = do
  logMessage $ divider symDivH Nothing
  logMessage $ "varElim" <+> pretty x <+> pretty b
  logMessage $ "φ  =" <+> pretty φ  
  ξ <- mapM (abstractVar x b) [r | r <- φ, x `elem` freeVars r]
  logMessage $ "ξ  =" <+> pretty ξ  
  let ξₘ = converge partialMeets (topExpr b : ξ)
  logMessage $ "ξₘ =" <+> pretty ξₘ
  if any containsBotAExpr ξₘ then do
    return Nothing
  else do
    let ψ₁ = [e₁ :=: e₂ | (e₁:es) <- tails ξₘ, e₂ <- es]    
    logMessage $ "ψ₁ =" <+> pretty ψ₁
    let ψ₂ = [r | r <- φ, x `notElem` freeVars r]
    logMessage $ "ψ₂ =" <+> pretty ψ₂
    let ψ = catMaybes $ List.nub $ map norm' $ ψ₁ ++ ψ₂
    logMessage $ "ψ  =" <+> pretty ψ
    return $ Just ψ

converge :: Eq a => (a -> a) -> a -> a
converge = until =<< ((==) =<<)

containsBotAExpr :: AExpr -> Bool
containsBotAExpr e = or [containsBot a | EAbs a <- Uniplate.universe e]
