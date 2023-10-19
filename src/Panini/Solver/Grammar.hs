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
solve (GCon s k c) | k `elem` kvars c = do
  logData c
  logMessage $ "Eliminate recursive grammar variable" <+> pretty k
  let c' = apply [(k,PTrue)] c
  solve (GCon s k c')

-- TODO: clean up
solve (GCon s k c) = do
  logMessage "Rewrite grammar constraint"
  logData c
  c' <- rewrite c
  logData c'
  logMessage "abstract string from rewritten constraints"
  g <- joins <$> (forM c' $ \ps -> do
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

rewrite :: Con -> Pan [[Rel]]
rewrite c0 = unwrapDNF <$> go c0
 where
  go :: Con -> Pan Pred
  go = \case
    CHead p      -> dnf p
    CAnd c1 c2   -> dnf =<< liftA2 (∧) (wrapDNF <$> rewrite c1) (wrapDNF <$> rewrite c2)    
    CAll x b p c -> dnf =<< PNot . PExists x b . PNot . PImpl p <$> wrapDNF <$> rewrite c
  
  dnf :: Pred -> Pan Pred
  dnf = \case
    PTrue                   -> return PTrue
    PFalse                  -> return PFalse    
    PRel r | isContra r     -> return PFalse
    PRel r                  -> return $ PRel r          
    PNot PTrue              -> return PFalse
    PNot PFalse             -> return PTrue    
    PNot (PRel r)           -> dnf $ PRel (inverse r)
    PNot (PNot x)           -> dnf x
    PNot (PAnd xs)          -> dnf $ POr (map PNot xs)
    PNot (POr xs)           -> dnf $ PAnd (map PNot xs)    
    PNot (PImpl a b)        -> dnf $ PAnd [a, PNot b]    
    PNot (PIff a b)         -> dnf $ PIff a (PNot b)  -- TODO: optimize arbitrary choice?
    PNot x                  -> dnf =<< PNot <$> dnf x        
    PImpl a b               -> dnf $ POr [PNot a, b]
    PIff a b                -> dnf $ POr [PAnd [a,b], PAnd [PNot a, PNot b]]    
    PAnd [x]                -> dnf x
    PAnd xs0 -> filter (/= PTrue) . nub' <$> mapM dnf xs0 >>= \case
      xs1 | null xs1        -> return PTrue
          | elem PFalse xs1 -> return PFalse
          | hasContra xs1   -> return PFalse
          | any isPOr xs1   -> dnf $ POr $ distributeAnds xs1
          | any isPAnd xs1  -> dnf $ PAnd $ associateAnds xs1
          | otherwise       -> return $ PAnd xs1
    POr [x]                 -> dnf x
    POr xs0 -> filter (/= PFalse) . nub' <$> mapM dnf xs0 >>= \case
      xs1 | null xs1        -> return PFalse
          | elem PTrue xs1  -> return PTrue
          | any isPOr xs1   -> dnf $ POr $ associateOrs xs1
          | otherwise       -> return $ POr xs1
    PExists x b p           -> wrapDNF <$> (mapMaybeM (varElim x b) =<< unwrapDNF <$> dnf p)
    PAppK _ _               -> impossible
  
-- | Associate nested conjunctions, e.g., a ∧ (b ∧ c) ∧ d ≡ a ∧ b ∧ c ∧ d.
associateAnds :: [Pred] -> [Pred]
associateAnds xs = case partition isPAnd xs of
  (ys,zs) -> concat [y | PAnd y <- ys] ++ zs
{-# INLINE associateAnds #-}

-- | Associate nested disjunctions, e.g., a ∨ (b ∨ c) ∨ d ≡ a ∨ b ∨ c ∨ d.
associateOrs :: [Pred] -> [Pred]
associateOrs xs = case partition isPOr xs of
  (ys,zs) -> concat [y | POr y <- ys] ++ zs
{-# INLINE associateOrs #-}

-- | Distribute a list of conjuncts over any nested disjunctions, resulting in a
-- list of disjuncts, e.g., a ∧ (b ∨ c) ∧ d ≡ (a ∧ b ∧ d) ∨ (a ∧ c ∧ d).
distributeAnds :: [Pred] -> [Pred]
distributeAnds xs = case partition isPOr xs of
  (ys,zs) -> map (PAnd . (zs ++)) $ sequence [y | POr y <- ys]
{-# INLINE distributeAnds #-}

-- | Whether a conjunction contains both a literal and its dual, e.g., a ∧ ¬a.
hasContra :: [Pred] -> Bool
hasContra xs = or [PRel (inverse r) `elem` xs | PRel r <- xs]
{-# INLINE hasContra #-}

-- | Whether a relation is contradictory, e.g., a = ⊥ or a ≠ ⊤.
isContra :: Rel -> Bool
isContra (e1 :=: e2) = containsBotAExpr e1 || containsBotAExpr e2
isContra (e1 :≠: e2) = containsTopAExpr e1 || containsTopAExpr e2
isContra _           = False
{-# INLINE isContra #-}

wrapDNF :: [[Rel]] -> Pred
wrapDNF = POr . map (PAnd . map PRel)
{-# INLINE wrapDNF #-}

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
      | (ys,zs) <- partition isPAnd xs, all isPRel zs = [unAnd y | PAnd y <- ys] ++ [[r] | PRel r <- zs]
      | otherwise = panic $ "expected all PAnd instead of" <+> pretty xs

nub' :: Hashable a => [a] -> [a]
nub' = HashSet.toList . HashSet.fromList
{-# INLINE nub' #-}

-------------------------------------------------------------------------------

-- TODO: clean up
-- TODO: update submission
-- | Algorithm 3 in OOPSLA'23 submission.
varElim :: Name -> Base -> [Rel] -> Pan (Maybe [Rel])
varElim x TUnit ps = return $ Just $ map (\r -> subst (ECon (U NoPV)) x r) ps
varElim x b ps = do
  -- logMessage $ "varElim" <+> pretty x <+> pretty b
  -- logData $  ps
  let (pxs, ps') = List.partition (elem x . freeVars) ps
  -- logData (pxs, ps')
  let (pxs', xvs) = partitionEithers $ map (maybeToEither (isolateVar x)) pxs
  -- logData (xvs, pxs')

  let bTop = topExpr b

  let x̂s₀ = Map.singleton [] bTop
  let refine x̂s xv = do
        -- logMessage $ "refine" <+> pretty xv
        let vs = freeVars xv
        let x̂ = fromMaybe bTop $ Map.lookup vs x̂s
        case x̂ ∧? xv of
          Just x̂' -> return $ Map.insert vs x̂' x̂s
          Nothing -> throwError $ MeetImpossible x̂ xv NoPV  
  x̂s <- foldM refine x̂s₀ xvs
  -- logData x̂s

  if any containsBotAExpr x̂s then do
    -- logMessage "Nothing"
    return Nothing
  else do
    let x̂s1 = map snd $ Map.toAscList x̂s
    let qs1 = List.nub $ map normRel $ [x̂₁ :=: x̂₂ | (x̂₁:rest) <- tails x̂s1, x̂₂ <- rest]
    -- logData qs1

    let qs2 = concatMap (\px' -> map (\x̂ -> subst x̂ x px') x̂s1) pxs'
    -- logData qs2

    let qs = qs1 ++ qs2 ++ ps'
    -- logData qs

    return $ Just qs

maybeToEither :: (a -> Maybe b) -> (a -> Either a b)
maybeToEither f = \x -> maybe (Left x) Right (f x)

containsBotAExpr :: AExpr -> Bool
containsBotAExpr e = or [containsBot a | EAbs a <- Uniplate.universe e]

containsTopAExpr :: AExpr -> Bool
containsTopAExpr e = or [containsTop a | EAbs a <- Uniplate.universe e]

-------------------------------------------------------------------------------

isPOr :: Pred -> Bool
isPOr (POr _) = True
isPOr _       = False
{-# INLINE isPOr #-}

isPAnd :: Pred -> Bool
isPAnd (PAnd _) = True
isPAnd _        = False
{-# INLINE isPAnd #-}

isPRel :: Pred -> Bool
isPRel (PRel _) = True
isPRel _        = False
{-# INLINE isPRel #-}
