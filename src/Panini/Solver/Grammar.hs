module Panini.Solver.Grammar 
  ( GCon(..)
  , grammarConstraints
  , gconKVar
  , solve
  ) where

import Algebra.Lattice
import Control.Monad
import Control.Monad.ST
import Data.Foldable
import Data.Generics.Uniplate.Operations qualified as Uniplate
import Data.Hashable
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.List (partition)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.STRef
import GHC.Generics
import Panini.Abstract.AValue
import Panini.Abstract.Semantics
import Panini.Pretty.Printer
import Panini.Solver.Assignment
import Panini.Solver.Constraints
import Panini.Syntax
import Prelude

-------------------------------------------------------------------------------

-- | A /grammar constraint/ is any constraint of the form @∀s:𝕊. κ(s) ⇒ c@.
-- Here, @κ@ is known as a /grammar variable/ and @c@ as a /grammar consequent/.
-- The string variable @s@, applied parameter to @κ@, is a free variable in @c@.
data GCon = GCon Name KVar Con
  deriving stock (Eq, Show, Read, Generic)

instance Hashable GCon

instance Pretty GCon where
  pretty (GCon x k c) = pretty $ CAll x TString (PAppK k [Var x]) c

-- | Returns all grammar constraints within the given constraint.
grammarConstraints :: Con -> HashSet GCon
grammarConstraints c0 = HashSet.fromList
  [GCon x k c | CAll x TString (PAppK k [Var y]) c <- Uniplate.universe c0
              , y == x
  ]

gconKVar :: GCon -> KVar
gconKVar (GCon _ k _) = k

-------------------------------------------------------------------------------

-- | Solve a grammar constraint @∀s:𝕊. κ(s) ⇒ c@, returning a solution for @κ@.
solve :: GCon -> Assignment
solve (GCon s k c) = Map.singleton k g'
  where    
    g = PRel . concretizeVar s . EAbs . AString
      $ joins
      $ map (meets . map (abstractStringVar s))  -- TODO
      $ unDNF
      $ rewrite c
    
    -- IMPORTANT: we need to substitute the free string variable s in the
    -- grammar solution with the generic κ parameter, so that later on we can
    -- apply without problems
    g' = subst (Var $ head $ kparams k) s g


-- TODO: either make this unnecessary or deal with errors gracefully
abstractStringVar :: Name -> Rel -> AString
abstractStringVar x p = case abstractVar x TString p of
  EAbs (AString s) -> s
  _                -> error "expected abstract string"

-------------------------------------------------------------------------------

rewrite :: Con -> DNF Rel
rewrite = \case
  CHead p      -> toDNF p
  CAnd c1 c2   -> rewrite c1 ∧ rewrite c2
  CAll x b p c -> case c of
    CAll x2 b2 p2 c2 -> varElimDNF x b $ rewrite $ CAll x2 b2 (p ∧ p2) c2
    CHead q          -> varElimDNF x b $ toDNF $ p ∧ q
    CAnd c1 c2       -> joins $ flip map (unDNF $ toDNF p) $ \p' ->
                          let c1' = varElimDNF x b $ DNF [p'] ∧ rewrite c1
                              c2' = varElimDNF x b $ DNF [p'] ∧ rewrite c2
                          in c1' ⟑ c2'

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
        _ -> error "impossible"
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
  PIff a b -> Just $ POr [PAnd [PNot a, PNot b], PAnd [a, b]]

  _ -> Nothing

unwrapDNF :: Pred -> [[Rel]]
unwrapDNF = \case
  POr  xs -> unOr xs
  PAnd ys -> [unAnd ys]
  PTrue   -> [[]]
  PFalse  -> []
  PRel r  -> [[r]]
  p -> error $ "expected POr/PAnd/PTrue/PFalse/PRel instead of " ++ showPretty p
  where
    unAnd ys
      | all isPRel ys = [y | PRel y <- ys]
      | otherwise = error $ "expected all PRel instead of " ++ showPretty ys
    unOr xs
      | all isPAnd xs = [unAnd ys | PAnd ys <- xs]
      | otherwise = error $ "expected all PAnd instead of " ++ showPretty xs

-------------------------------------------------------------------------------

varElimDNF :: Name -> Base -> DNF Rel -> DNF Rel
varElimDNF x b = DNF . mapMaybe (varElim x b) . unDNF

-- | Algorithm 3 in OOPSLA'23 submission.
varElim :: Name -> Base -> [Rel] -> Maybe [Rel]
varElim _ TUnit ps = Just ps  -- TODO
varElim x b ps = runST $ do
  let bTop = topExpr b
  x̂sRef <- newSTRef $ Map.singleton [x] bTop

  forM_ [(p,v̄) | p <- ps, let v̄ = freeVars p, x `elem` v̄] $ \(p,v̄) -> do
    x̂₀ <- fromMaybe bTop . Map.lookup v̄ <$> readSTRef x̂sRef
    let x̂₁ = abstractVar x b p
    case x̂₀ ∧? x̂₁ of
      Just x̂ -> modifySTRef' x̂sRef $ Map.insert v̄ x̂
      Nothing -> error $ "cannot meet " ++ showPretty x̂₀ ++ " with " ++ showPretty x̂₁

  x̂Self <- fromJust . Map.lookup [x] <$> readSTRef x̂sRef
  case x̂Self of
    EAbs (ABool   a) | isBot a -> return Nothing
    EAbs (AInt    a) | isBot a -> return Nothing
    EAbs (AString a) | isBot a -> return Nothing
    _ -> do
      x̂s' <- filter (([x] /=) . fst) . Map.assocs <$> readSTRef x̂sRef
      let (v̄ₘ,x̂ₘ) = if null x̂s' 
                      then ([x], x̂Self) 
                      else head x̂s'  -- TODO: pick "smallest" meet

      let qs = map (substExpr x̂ₘ x) $ filter ((v̄ₘ /=) . freeVars) ps
      return $ Just qs

substExpr :: Expr -> Name -> Rel -> Rel
substExpr x̂ x =  Uniplate.transformBi $ \case
  EVal (Var y) | y == x -> x̂
  e                     -> e
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
