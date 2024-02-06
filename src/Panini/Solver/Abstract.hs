{-# LANGUAGE OverloadedLists #-}

module Panini.Solver.Abstract
  ( PreCon(..)
  , allPreCons
  , preConKVar
  , solve
  ) where

import Panini.Solver.Constraints
import Panini.Syntax
import Panini.Solver.Assignment
import Panini.Monad
import Data.Hashable
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Set qualified as Set
import GHC.Generics
import Data.Generics.Uniplate.Operations qualified as Uniplate
import Panini.Abstract.AExpr
import Panini.Abstract.AValue
import Data.List qualified as List
import Panini.Pretty
import Panini.Panic
import Panini.Abstract.Semantics
import Data.Maybe
import Algebra.Lattice
import Control.Monad.Extra
import Prelude
import Data.Map.Strict qualified as Map
import Data.Graph qualified as Graph
import Data.Foldable
import Panini.Solver.Simplifier
import Panini.Abstract.AString qualified as AString

-------------------------------------------------------------------------------

-- A /precondition constraint/ is any constraint of the form @∀x:b. κ(x) => c@,
-- where @κ@ does not occur in @c@ in head position (but may occur otherwise).
-- In this context, @κ@ is known as a /precondition variable/. Note that the
-- variable @x@, the parameter applied to @κ@, is a free variable in @c@.
data PreCon = PreCon Name Base KVar Con
  deriving stock (Eq, Show, Read, Generic)

instance Hashable PreCon

instance Pretty PreCon where
  pretty (PreCon x b k c) = pretty $ CAll x b (PAppK k [EVar x]) c

-- | Return all precondition constraints within the given constraint.
allPreCons :: Con -> HashSet PreCon
allPreCons c0 = HashSet.fromList 
  [ PreCon x b k c 
  | CAll x b (PAppK k [EVar y]) c <- Uniplate.universe c0, y == x
  , null [k' | CHead (PAppK k' _) <- Uniplate.universe c, k' == k]
  , freeVars c `Set.isSubsetOf` [x]
  ]

preConKVar :: PreCon -> KVar
preConKVar (PreCon _ _ k _) = k

-------------------------------------------------------------------------------

-- | Solve all precondition variables in a constraint, returning the solutions.
--
-- Our approach abstractly interprets (parts of) the constraint to arrive at the
-- weakest solution for each precondition variable. The final solution is always
-- a concrete predicate.
--
-- If the underlying abstract domains are not powerful enough to capture all
-- values expressed by the constraint for any particular variable, an
-- 'AbstractionImpossible' error will be thrown. If an abstracted value cannot
-- finally be re-concretized, a 'ConcretizationImpossible' error will be thrown.
--
-- Our approach first orders constraints by their κ variable dependencies and
-- then tries to solve them sequentially, applying intermediate solutions on the
-- way. If there are multiple solutions for the same variable (i.e., if some κᵢ
-- appears multiple times), we take their 'meet'.
solve :: Con -> Pan Assignment
solve c0 = do
  logMessage "Use abstract interpretation to infer weakest preconditions"
  pcs1 <- topoSortPreCons (allPreCons c0) § "Sort precondition variables"
  Map.map snd <$> foldM solve' mempty pcs1
 where
  -- solve' s (PreCon _x TUnit k _c) = do
  --   return $ Map.insert k (AInt bot, PTrue) s -- TODO

  solve' s (PreCon x b k c) = do
    logMessage $ "Solve" <+> pretty (PAppK k [EVar x])
    c' <- apply (Map.map snd s) c   § "Apply partial solution"
    a1 <- solve1 (PreCon x b k c')
    a2 <- meet' a1 (Map.lookup k s) § "Meet with previous abstract solution"
    p  <- concretizeVar (head $ kparams k) b a2
    return $ Map.insert k (a2,p) s
  
  meet' a1 = \case
    Nothing -> a1
    Just (a0,_) -> case a0 ∧? a1 of
      Just a2 -> a2
      Nothing -> impossible

topoSortPreCons :: Foldable t => t PreCon -> [PreCon]
topoSortPreCons pcs = 
  Graph.flattenSCCs $ Graph.stronglyConnComp $ map adj $ toList pcs
 where    
  adj g@(PreCon _ _ k c) = (g, k2i k, map k2i $ Set.toList $ relevantKVars c)
  relevantKVars c        = Set.intersection (kvars c) gvars
  gvars                  = Set.fromList [k | PreCon _ _ k _ <- toList pcs]
  k2i (KVar i _)         = i


-- | Solve a single precondition constraint, resulting in an abstract value.
solve1 :: PreCon -> Pan AValue
solve1 = \case
  -- TODO: verify this
  -- Trick to solve simple recursions: eliminate recursive κ applications by
  -- replacing them simply with true, then try to solve normally. If the
  -- recursions did not actually affect the precondition, the solution will
  -- validate the VC, which still includes the recursive applications.
  -- Otherwise, the recursion must have encoded some information that was lost
  -- by this simple elimination and the VC will be judged invalid.
  PreCon x b k c | not $ null $ kvars c -> do
    let st = Map.fromList [(k2, PTrue) | k2 <- toList (kvars c)]
    c1 <- apply st c      § "Set nested" <+> kappa <+> "variables true"
    c2 <- simplifyCon c1  § "Simplify constraint"
    solve1 $ PreCon x b k c2

  PreCon x TUnit _ c -> do
    c1 <- rewrite c
    rs <- dnf c1 § "Convert to DNF"
    logMessage $ "Abstract unit variable" <+> pretty x
    AUnit <$> joins <$> mapM ((meets <$>) . mapM (abstractUnit x)) rs

  PreCon x TBool _ c -> do
    c1 <- rewrite c
    rs <- dnf c1 § "Convert to DNF"
    logMessage $ "Abstract bool variable" <+> pretty x
    ABool <$> joins <$> mapM ((meets <$>) . mapM (abstractBool x)) rs
  
  PreCon x TInt _ c -> do
    c1 <- rewrite c
    rs <- dnf c1 § "Convert to DNF"
    logMessage $ "Abstract integer variable" <+> pretty x
    AInt <$> joins <$> mapM ((meets <$>) . mapM (abstractInt  x)) rs
  
  PreCon x TString _ c -> do
    c1 <- rewrite c
    rs <- dnf c1 § "Convert to DNF"
    logMessage $ "Abstract string variable" <+> pretty x
    s0 <- joins <$> mapM ((meets <$>) . mapM (abstractStr  x)) rs
    s1 <- AString.simplify s0  § "Simplify abstract string"
    return $ AString s1


-- TODO: make this type wrangling stuff unnecessary

abstractUnit :: Name -> Rel -> Pan AUnit
abstractUnit x r = do
  e <- abstractVar x TUnit r
  case e of
    EUnitA a -> return a
    _        -> panic $ "abstractUnit: unexpected" <+> pretty e

abstractBool :: Name -> Rel -> Pan ABool
abstractBool x r = do
  e <- abstractVar x TBool r
  case e of
    EBoolA a -> return a
    _        -> panic $ "abstractBool: unexpected" <+> pretty e

abstractInt :: Name -> Rel -> Pan AInt
abstractInt x r = do
  e <- abstractVar x TInt r
  case e of
    EIntA a -> return a
    _       -> panic $ "abstractInt: unexpected" <+> pretty e

abstractStr :: Name -> Rel -> Pan AString
abstractStr x r = do
  e <- abstractVar x TString r
  case e of
    EStrA a -> return a
    _       -> panic $ "abstractStr: unexpected" <+> pretty e


-------------------------------------------------------------------------------

rewrite :: Con -> Pan Pred
rewrite c0 = do
  c1 <- elimAll c0 § "Eliminate ∀"
  logMessage "Eliminate ∃"
  c2 <- elimExists c1
  logData c2
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
fromDNF = joins . map (meets . map PRel)

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

-- | Independently normalize each side of a relation.
normRel :: Rel -> Rel
normRel (Rel op e1 e2) = Rel op (norm e1) (norm e2)

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
    let ψ₁ = [e₁ :=: e₂ | (e₁:es) <- List.tails ξₘ, e₂ <- es]    
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
