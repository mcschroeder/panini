module Panini.Logic.Solver.Grammar (infer) where

import Control.Monad
import Control.Monad.ST
import Data.Foldable
import Data.Generics.Uniplate.Operations qualified as Uniplate
import Data.HashSet qualified as HashSet
import Data.List (partition)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.STRef
import Debug.Trace
import Panini.Abstract.ABool
import Panini.Abstract.AString
import Panini.Abstract.AValue
import Panini.Abstract.Interpretation
import Panini.Algebra.Lattice
import Panini.Logic.Constraints
import Panini.Logic.Predicates
import Panini.Names
import Panini.Pretty.Printer
import Panini.Primitives
import Panini.Substitution
import Prelude

-------------------------------------------------------------------------------

infer :: Name -> Con -> Pred
infer s = PPred . concretizeVar s . PAbs . AString
        . foldl' (∨) (⊥) 
        . map (foldl' (∧) (⊤) . map (abstractStringVar s))  -- TODO
        . toPredsDNF
        . rewrite

-- TODO: either make this unnecessary or deal with errors gracefully
abstractStringVar :: Name -> Pred2 -> AString
abstractStringVar x p = case abstractVar x TString p of
  PAbs (AString s) -> s
  _                -> error "expected abstract string"

-------------------------------------------------------------------------------

-- TODO: ensure PRel at bottom
toPredsDNF :: Pred -> [[Pred2]]
toPredsDNF (POr ps) | all isPAnd ps = [[y | PPred y <- xs] | PAnd xs <- ps]
toPredsDNF (PAnd xs) | all isPRel xs = [[y | PPred y <- xs]]
toPredsDNF c = error $ "expected (POr [PAnd _]) instead of " ++ showPretty c


-------------------------------------------------------------------------------


tracer2 :: (Pretty a, Pretty b) => (a -> b) -> a -> b
tracer2 f x = runST $ do
  traceM $ replicate 40 '-'
  traceM $ showPretty x
  traceM $ "⇝"
  let x' = f x
  traceM $ showPretty x'
  traceM $ replicate 40 '-'
  return $ x'

rewrite :: Con -> Pred  -- DNF?
rewrite = tracer2 $ \case
  CHead p -> rewrite2 p
  CAnd c1 c2 -> rewrite2 $ PAnd [rewrite c1, rewrite c2]  
  CAll _ _ PFalse _ -> PTrue
  CAll x b (POr ps) c -> rewrite2 $ POr $ map (\p' -> rewrite $ CAll x b p' c) ps  
  CAll x b p (CAnd c1 c2) -> rewrite2 $ PAnd [rewrite $ CAll x b p c1, rewrite $ CAll x b p c2]
  CAll x b p1 (CHead p2) -> rewrite2 $ varElimDNF x b $ rewrite2 $ PAnd [p1,p2]
  CAll x1 b1 p1 (CAll x2 b2 p2 c2) -> rewrite2 $ varElimDNF x1 b1 $ rewrite $ CAll x2 b2 (rewrite2 $ PAnd [p1,p2]) c2

unwrapPreds :: [Pred] -> [Pred2]
unwrapPreds [] = []
unwrapPreds (PPred p : xs) = p : unwrapPreds xs
unwrapPreds _ = error "expected PPred"

varElimDNF :: Name -> Base -> Pred -> Pred
varElimDNF x b (POr xs) | all isPAnd xs =
  POr [PAnd $ map PPred $ varElim x b $ unwrapPreds ys | PAnd ys <- xs]
varElimDNF x b (PAnd xs) | all isPRel xs =
  POr [PAnd (map PPred $ varElim x b $ unwrapPreds xs)]
varElimDNF _ _ PTrue = PTrue
varElimDNF _ _ PFalse = PFalse
varElimDNF _ _ p = error $ "expected DNF instead of " ++ showPretty p


-- TODO: this should always return well-formed DNF
rewrite2 :: Pred -> Pred
rewrite2 = Uniplate.rewrite $ \case
  PAnd xs
    | any (== PFalse) xs -> Just PFalse
    | any (== PTrue) xs -> Just $ PAnd $ List.filter (/= PTrue) xs
    | any isPAnd xs ->
      let (ys, zs) = partition isPAnd xs
          ys' = mconcat [y | PAnd y <- ys]
          zs' = zs
      in Just $ PAnd $ ys' ++ zs'
    | any isPOr xs -> case partition isPOr xs of
        (POr ys : yys, zs) -> Just $ POr $ [PAnd $ y : (yys ++ zs) | y <- ys]
        _ -> undefined -- TODO
    | or [neg x `elem` xs | x <- xs] -> Just PFalse -- TODO: this needs a normal form / eval
    | HashSet.size (HashSet.fromList xs) < length xs -> Just $ PAnd $ List.nub xs  -- TODO
    | [x] <- xs -> Just x
    
  POr xs
    | any (== PTrue) xs -> Just PTrue
    | any (== PFalse) xs -> Just $ POr $ List.filter (/= PFalse) xs
    | any isPOr xs -> 
      let (ys, zs) = partition isPOr xs
          ys' = mconcat [y | POr y <- ys]
          zs' = zs
      in Just $ POr $ ys' ++ zs'
    | [x] <- xs -> Just x

  PNot (POr xs) -> Just $ PAnd $ map neg xs  
  PNot (PAnd xs) -> Just $ POr $ map neg xs

  PImpl a b -> Just $ POr [neg a, b]

  PIff a b -> Just $ POr [ PAnd [neg a, neg b], PAnd [a, b]]

  p@(PPred _) -> evalP p

  _ -> Nothing

instance Complementable Pred where
  neg (PPred (PRel r e1 e2)) = 
    let p' = PPred (PRel (invRel r) e1 e2)
    in case evalP p' of
      Nothing -> p'
      Just p'' -> p''
  neg (PNot p) = p
  neg p = PNot p


isPOr :: Pred -> Bool
isPOr (POr _) = True
isPOr _ = False

isPAnd :: Pred -> Bool
isPAnd (PAnd _) = True
isPAnd _ = False

isPRel :: Pred -> Bool
isPRel (PPred (PRel _ _ _)) = True
isPRel _ = False


-- | Algorithm 3 in OOPSLA'23 submission.
varElim :: Name -> Base -> [Pred2] -> [Pred2]
varElim x b ps = runST $ do

  -- traceM $ "varElim " ++ showPretty x ++ " " ++ showPretty b ++ " " ++ showPretty ps

  let bTop = topExpr b
  x̂sRef <- newSTRef $ Map.empty
  
  forM_ [(p,v̄) | p <- ps, let v̄ = freeVars p, x `elem` v̄] $ \(p,v̄) -> do
    x̂₀ <- fromMaybe bTop <$> Map.lookup v̄ <$> readSTRef x̂sRef
    let x̂₁ = abstractVar x b p    
    case x̂₀ ∧? x̂₁ of
      Just x̂ -> modifySTRef' x̂sRef $ Map.insert v̄ x̂
      Nothing -> error $ "cannot meet " ++ showPretty x̂₀ ++ " with " ++ showPretty x̂₁

  x̂s' <- filter (([x] /=) . fst) <$> Map.assocs <$> readSTRef x̂sRef  
  (v̄ₘ,x̂ₘ) <- if null x̂s' 
    then do
      x̂Self <- fromJust <$> Map.lookup [x] <$> readSTRef x̂sRef
      pure ([x], x̂Self) 
    else 
      pure $ head x̂s'  -- TODO: pick "smallest" meet

  -- traceM $ showPretty $ "varElim" <+> pretty x <+> pretty b <+> pretty ps <+> pretty (v̄ₘ,x̂ₘ)

  let qs = map (substExpr x̂ₘ x) $ filter ((v̄ₘ /=) . freeVars) ps

  -- traceM $ "  " ++ showPretty qs

  return qs


-- TODO: integrate into existing substitution architecture
substExpr :: PExpr -> Name -> Pred2 -> Pred2
substExpr x̂ x p = case p of
  PReg _e _re -> undefined -- TODO
  PRel r e1 e2 -> PRel r (go e1) (go e2)
 where
  go = \case
    PVal (Var y) | y == x -> x̂
    PVal (Var y) -> PVal (Var y)
    PVal (Con c) -> PVal (Con c)
    PAbs a -> PAbs a
    PAdd e1 e2 -> PAdd (go e1) (go e2)
    PMul e1 e2 -> PMul (go e1) (go e2)
    PSub e1 e2 -> PSub (go e1) (go e2)
    PStrLen e -> PStrLen (go e)
    PStrAt e1 e2 -> PStrAt (go e1) (go e2)
    PStrSub e1 e2 e3 -> PStrSub (go e1) (go e2) (go e3)
    PFun f xs -> PFun f (map go xs)
    PNot2 e -> PNot2 (go e)

evalP :: Pred -> Maybe Pred
-- evalP (TReg e re) = case evalE e of
--   TCon (S s _)     -> Just $ undefined s re -- TODO: regex inclusion
--   PAbs (AString s) -> Just $ undefined s re -- TODO: abstract regex inclusion?
--   e' | e' /= e     -> Just $ TPred $ TReg e' re
--   _                -> Nothing

evalP (PPred (PRel r e1 e2)) = case (evalE e1, evalE e2) of

  (PVar x, PCon (B b pv)) -> case r of
    Ne -> Just $ PPred $ PRel Eq (PVar x) (PCon (B (not b) pv))
    _ -> Nothing

  (PNot2 x@(PVar _), e2'@(PCon (B _ _))) -> Just $ PPred $ PRel (invRel r) x e2'

  (PCon (B b1 _), PCon (B b2 _)) -> case r of
    Eq | b1 == b2 -> Just PTrue
    Ne | b1 /= b2 -> Just PTrue
    _             -> Just PFalse  -- TODO: might be hiding type error case
  
    -- TODO: comparison with abstract values is an abstract operation?
    -- {T,F} = T  ??? true or false?

  -- TODO: hardcoded hack; replace with general eval
  (PAbs (ABool b1), PCon (B b2 _)) -> case concreteBool b1 of
    Nothing  -> Nothing
    Just b1' -> case r of
      Eq | b1' == b2 -> Just PTrue
      Ne | b1' /= b2 -> Just PTrue
      _              -> Just PFalse  -- TODO: might be hiding type error case
    
  (PCon (I i1 _), PCon (I i2 _)) -> case r of
    Eq | i1 == i2 -> Just PTrue
    Ne | i1 /= i2 -> Just PTrue
    Gt | i1 >  i2 -> Just PTrue
    Ge | i1 >= i2 -> Just PTrue
    Lt | i1 <  i2 -> Just PTrue
    Le | i1 <= i2 -> Just PTrue
    _             -> Just PFalse  -- TODO: might be hiding type error case
  
  (PCon (S s1 _), PCon (S s2 _)) -> case r of
    Eq | s1 == s2 -> Just PTrue
    Ne | s1 /= s2 -> Just PTrue
    _             -> Just PFalse  -- TODO: might be hiding type error case
  
  (e1', e2') 
    | e1' /= e1 || e2' /= e2 -> Just $ PPred $ PRel r e1' e2'
    | otherwise              -> Nothing

evalP _ = Nothing

evalE :: PExpr -> PExpr
evalE = \case
  PNot2 (PCon (B b pv)) -> PCon (B (not b) pv)
  PNot2 (PAbs a) -> evalE $ PAbs (neg a)
  e -> e

