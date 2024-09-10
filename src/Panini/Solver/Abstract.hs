{-# LANGUAGE OverloadedLists #-}

module Panini.Solver.Abstract
  ( PreCon(..)
  , allPreCons
  , preConKVar
  , solve
  ) where

import Algebra.Lattice
import Control.Monad.Extra
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable
import Data.Generics.Uniplate.Operations qualified as Uniplate
import Data.Graph qualified as Graph
import Data.Hashable
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import GHC.Generics
import Panini.Abstract.AString qualified as AString
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
import System.Time.Extra

-------------------------------------------------------------------------------

-- A /precondition constraint/ is any constraint of the form @∀x:b. κ(x) => c@.
-- In this context, @κ@ is known as a /precondition variable/. Note that the
-- variable @x@, the parameter applied to @κ@, is a free variable in @c@.
data PreCon = PreCon Name Base KVar Con
  deriving stock (Eq, Show, Read, Generic)

instance Hashable PreCon

instance Pretty PreCon where
  pretty (PreCon x b k c) = pretty $ CAll x b (PAppK k [EVar x]) c

-- | Return all precondition constraints within the given constraint.
allPreCons :: Con -> HashSet PreCon
allPreCons c0 = HashSet.fromList $
  [ PreCon x b k c | CAll x b p c <- Uniplate.universe c0, k <- preConK x p ]  
 where
  preConK x = \case
    PAppK k [EVar y] | x == y -> [k]
    POr ps                    -> concatMap (preConK x) ps
    _                         -> []

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
  k2i (KVar i _ _)       = i


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

  PreCon x b _ c -> do
    c1 <- qelim c
    c2 <- nnf c1 § "Convert to NNF"
    c3 <- simplifyPred c2 § "Simplify predicate"
    logMessage $ "Abstract" <+> pretty x <> colon <> pretty b
    q <- abstractNNF x b c3
    logData q
    return q

abstractNNF :: Name -> Base -> Pred -> Pan AValue
abstractNNF x b = \case
  PTrue   -> return $ topValue b
  PFalse  -> return $ botValue b
  PRel r  -> simplify' =<< abstractVar' x b r
  PAnd xs -> do vs <- mapM (abstractNNF x b) xs
                v <- simplify' $ meets' b vs
                logMessage $ "⋀" <> pretty vs <+> symEq <+> pretty v
                return v    
  POr xs  -> do vs <- mapM (abstractNNF x b) xs
                v <- simplify' $ joins' b vs
                logMessage $ "⋁" <> pretty vs <+> symEq <+> pretty v
                return v
  p       -> panic $ "abstractNNF: unexpected" <+> pretty p

simplify' :: AValue -> Pan AValue
simplify' (AString s) = do
  t <- gets regexTimeout
  r <- liftIO $ timeout t $ return $! AString.simplify s
  case r of
    Just s' -> return $ AString s'
    Nothing -> return $ AString s

simplify' a = pure a

meets' :: Base -> [AValue] -> AValue
meets' b xs = case b of
  TUnit   -> AUnit   $ meets $ map unsafeUnwrapAUnit xs
  TBool   -> ABool   $ meets $ map unsafeUnwrapABool xs
  TInt    -> AInt    $ meets $ map unsafeUnwrapAInt xs
  TChar   -> AChar   $ meets $ map unsafeUnwrapAChar xs  
  TString -> AString $ meets $ map unsafeUnwrapAString xs

joins' :: Base -> [AValue] -> AValue
joins' b xs = case b of
  TUnit   -> AUnit   $ joins $ map unsafeUnwrapAUnit xs
  TBool   -> ABool   $ joins $ map unsafeUnwrapABool xs
  TInt    -> AInt    $ joins $ map unsafeUnwrapAInt xs
  TChar   -> AChar   $ joins $ map unsafeUnwrapAChar xs
  TString -> AString $ joins $ map unsafeUnwrapAString xs

unsafeUnwrapAUnit :: AValue -> AUnit
unsafeUnwrapAUnit (AUnit a) = a
unsafeUnwrapAUnit a = panic $ "unsafeUnwrapAUnit: unexpected" <+> pretty a

unsafeUnwrapABool :: AValue -> ABool
unsafeUnwrapABool (ABool a) = a
unsafeUnwrapABool a = panic $ "unsafeUnwrapABool: unexpected" <+> pretty a

unsafeUnwrapAInt :: AValue -> AInt
unsafeUnwrapAInt (AInt a) = a
unsafeUnwrapAInt a = panic $ "unsafeUnwrapAInt: unexpected" <+> pretty a

unsafeUnwrapAChar :: AValue -> AChar
unsafeUnwrapAChar (AChar a) = a
unsafeUnwrapAChar a = panic $ "unsafeUnwrapAChar: unexpected" <+> pretty a

unsafeUnwrapAString :: AValue -> AString
unsafeUnwrapAString (AString a) = a
unsafeUnwrapAString a = panic $ "unsafeUnwrapAString: unexpected" <+> pretty a

-------------------------------------------------------------------------------

qelim :: Con -> Pan Pred
qelim c0 = do
  c1 <- elimAll c0 § "Eliminate ∀"
  c2 <- elimExists c1
  logData c2
  return c2
 where
  elimAll :: Con -> Pred
  elimAll = \case
    CHead p       -> p
    CAnd c1 c2    -> elimAll c1 ∧ elimAll c2
    CAll x t p c
      | x `notFreeIn` p, x `notFreeIn` c      -> PImpl p $ elimAll c
      | otherwise -> PNot $ PExists x t $ PNot $ PImpl p $ elimAll c

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
    PAppK _ _     -> impossible
    PExists x t p -> do p' <- elimExists p
                        logMessage $ "Eliminate ∃" <> pretty x
                        logData $ PExists x t p'
                        q <- joins <$> mapM (qelim1 x t) (dnf p')
                        logData q
                        return q

nnf :: Pred -> Pred
nnf = \case
  PTrue            -> PTrue
  PFalse           -> PFalse
  PRel r           -> PRel r
  PNot PTrue       -> PFalse
  PNot PFalse      -> PTrue
  PNot (PRel r)    -> PRel (inverse r)
  PNot (PNot x)    -> x
  PNot (PAnd xs)   -> nnf $ POr (map PNot xs)
  PNot (POr xs)    -> nnf $ PAnd (map PNot xs)
  PNot (PImpl a b) -> nnf $ PAnd [a, PNot b]
  PNot (PIff a b)  -> nnf $ PIff a (PNot b)
  PImpl a b        -> nnf $ POr [PNot a, b]
  PIff a b         -> nnf $ POr [PAnd [a,b], PAnd [PNot a, PNot b]]
  PAnd xs          -> PAnd (map nnf xs)
  POr xs           -> POr (map nnf xs)
  _                -> impossible

dnf :: Pred -> [[Rel]]
dnf p0 = case nnf p0 of
  PTrue   -> [[]]
  PFalse  -> []  
  PRel r  -> [[r]]
  PAnd xs -> nubOrd $ map nubOrd $ map concat $ sequence $ map dnf xs
  POr xs  -> nubOrd $ map nubOrd $ concat $ map dnf xs
  _       -> impossible

-------------------------------------------------------------------------------

-- | Eliminate a single (existentially quantified) variable from a conjunction
-- of relations, returning a predicate that is logically equivalent to the input
-- but no longer contains the given variable.
--
--     𝔐 ⊧ qelim1 x b R  ⟺  𝔐 ⊧ ∃(x:b). R 
--
qelim1 :: Name -> Base -> [Rel] -> Pan Pred
qelim1 x b φ = do
  logMessage $ divider symDivH Nothing
  logMessage $ "qelim1" <+> pretty x <+> pretty b
  logMessage $ "φ ←" <+> pretty φ  
  ξ <- mapM (abstractVar x b) [r | r <- φ, x `elem` freeVars r]
  logMessage $ "ξ ←" <+> pretty ξ  
  let ψ₁ = [e₁ :=: e₂ | (e₁:es) <- List.tails ξ, e₂ <- es]
  let ψ₂ = [r | r <- φ, x `notElem` freeVars r]    
  ψ <- filter (taut /=) <$> nubOrd <$> mapM normRelM (ψ₁ ++ ψ₂)
  logMessage $ "ψ ←" <+> pretty ψ
  if any (== cont) ψ then do
    logMessage "↯"
    return PFalse
  else
    return $ meets $ map PRel ψ

normRelM :: Rel -> Pan Rel
normRelM r = do
  let r' = normRel r
  unless (r' == r) $ logMessage $ pretty r <+> " ⇝ " <+> pretty r'
  return r'
