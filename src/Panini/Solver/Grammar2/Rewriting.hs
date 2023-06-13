module Panini.Solver.Grammar2.Rewriting where

import Data.Generics.Uniplate.Operations qualified as Uniplate
import Panini.Algebra.Lattice
import Panini.Solver.Grammar2.Tree
import Panini.Syntax
import Prelude
import Control.Monad.ST
import Data.Map.Strict qualified as Map
import Data.STRef
import Control.Monad
import Data.Maybe
import Panini.Solver.Grammar2.Abstract
import Data.Foldable (minimumBy, toList)
import Data.Function
import Panini.Pretty.Printer
import Debug.Trace
import Panini.Pretty.Graphviz
import Data.List (partition)
import Panini.Solver.Abstract.ABool
import Data.HashSet qualified as HashSet
import Data.List qualified as List

-- this is very different from Uniplate.rewrite!
traceRewrite :: (Tree -> Maybe Tree) -> Tree -> Tree
traceRewrite f t0 = runST $ go 0 t0
 where
  go :: Int -> Tree -> ST a Tree
  go !n t = do
    _ <- pure $! traceGraph (fn n) t
    let !t' = Uniplate.transform (\t1 -> fromMaybe t1 (f t1)) t
    if t' == t
      then do
        _ <- pure $! traceGraph (fn (n + 1)) t'
        return t'
      else
        go (n + 1) t'
  
  fn n = "rewrite-" ++ show n ++ ".svg"  


tracer :: (Pretty a, Pretty b) => (a -> Maybe b) -> a -> Maybe b
tracer f x = runST $ do
  case f x of
    Nothing -> return Nothing
    Just x' -> do
      traceM $ replicate 40 '-'
      traceM $ showPretty x
      traceM $ "⇝"
      traceM $ showPretty x'
      traceM $ replicate 40 '-'
      return $ Just x'

endtrace :: (GraphViz a, Pretty a) => a -> a
endtrace !x = runST $ do
  traceM $ replicate 40 '='
  traceM $ "FINAL TREE"
  traceM $ showPretty $ traceGraph ("trace_FINAL.svg") x
  traceM $ replicate 40 '='
  return x


isImp :: Tree -> Bool
isImp (TImp _ _) = True
isImp _          = False

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



varElimDNF :: Name -> Base -> Pred -> Pred
varElimDNF x b (POr xs) | all isPAnd xs =
  POr $ map (\(PAnd ys) -> PAnd $ varElim x b ys) xs
varElimDNF x b (PAnd xs) | all isPRel xs =
  POr [PAnd (varElim x b xs)]
varElimDNF _ _ PTrue = PTrue
varElimDNF _ _ PFalse = PFalse
varElimDNF _ _ p = error $ "expected DNF instead of " ++ showPretty p

--   CAll x b p c ->
--     let p' = rewrite2 p
--         c' = rewrite (CAnd (CHead p') c)        
--     in case c' of
--       CHead (POr xs) | all isPAnd xs, and [all isPRel ys | PAnd ys <- xs] ->
--         CHead $ POr $ map (\(PAnd ys) -> PAnd $ varElim x b ys) xs            
--       _ -> error $ "expected CHead of DNF pred instead of " ++ showPretty c'
  
--   CAnd c1 c2 -> case (rewrite c1, rewrite c2) of
--     (CHead p1, CHead p2) -> rewrite $ CHead (PAnd [p1,p2])
--     (CHead p1, CAll x b p2 c) ->

--   CAnd (CHead p1) (CHead p2) -> CHead (PAnd [p1,p2])
--   CAnd (CHead p1) (CAll x b p2 c) -> CAll x b (PAnd [p1,p2]) c
--   CAnd (CHead p1) (CAnd c2 c3) ->

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
    | any isPOr xs ->
      let (POr ys : yys, zs) = partition isPOr xs
          cs = [PAnd $ y : (yys ++ zs) | y <- ys]
      in Just $ POr cs
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

  p@(PRel _ _ _) -> evalP p

  _ -> Nothing

instance Complementable Pred where
  neg (PRel r e1 e2) = 
    let p' = PRel (invRel r) e1 e2
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
isPRel (PRel _ _ _) = True
isPRel _ = False


-- -- | Rewriting from § 4.1 in OOPSLA'23 submission.
-- rewrite' :: Tree -> Tree
-- rewrite' = (endtrace .) $ Uniplate.rewrite $ tracer $ \case  
--   TAnd xs
--     | any (== TFalse) xs -> Just TFalse
--     | any (== TTrue) xs -> Just $ TAnd $ HashSet.delete TTrue xs
--     | any isAnd xs ->
--       let (ys, zs) = partition isAnd $ toList xs
--           ys' = HashSet.unions [y | TAnd y <- ys]
--           zs' = HashSet.fromList zs
--       in Just $ TAnd $ HashSet.union ys' zs'
--     | any isOr xs ->
--       let (TOr ys : yys, zs) = partition isOr $ toList xs
--           cs = [TAnd $ HashSet.fromList $ y : (yys ++ zs) | y <- toList ys]
--       in Just $ TOr $ HashSet.fromList cs
--     | or [neg x `HashSet.member` xs | x <- toList xs] -> Just TFalse
--     | [x] <- toList xs -> Just x
    
--   TOr xs
--     | any (== TTrue) xs -> Just TTrue
--     | any (== TFalse) xs -> Just $ TOr $ HashSet.delete TFalse xs
--     | any isOr xs -> 
--       let (ys, zs) = partition isOr $ toList xs
--           ys' = HashSet.unions [y | TOr y <- ys]
--           zs' = HashSet.fromList zs
--       in Just $ TOr $ HashSet.union ys' zs'
--     | [x] <- toList xs -> Just x
    
--     -- TODO: | merge subsets
  
--   TNeg (TPred p) -> Just $ TPred (neg p)
--   TNeg (TOr xs) -> Just $ TAnd $ HashSet.map neg xs  
--   -- TNeg (TAnd xs) -> 
--   --     Just $ TOr $ HashSet.fromList
--   --          $ map (TAnd . HashSet.fromList)
--   --          $ tail
--   --          $ traverse (\x -> [x, neg x]) 
--   --          $ toList xs
--   TNeg (TAnd xs) 
--     | all isPred xs -> 
--       Just $ TOr $ HashSet.fromList
--            $ map (TAnd . HashSet.fromList)
--            $ tail
--            $ traverse (\x -> [TPred x, TPred $ neg x]) 
--            $ [x | TPred x <- toList xs]
--   TNeg (TAnd xs) -> Just $ TOr $ HashSet.map neg xs
--   -- TNeg (TAnd xs) | all isPred xs -> Nothing
--   -- TNeg (TAnd xs)
--   --   | all isPred xs -> Just $ TAnd $ HashSet.map neg xs
--   --   | otherwise     -> Just $ TOr  $ HashSet.map neg xs

--   -- TImp (TOr xs) b 
--   --   | all isAnd xs
--   --   , and [ all isPred ys | TAnd ys <- toList xs] 
--   --   -> Just $ TOr $ HashSet.fromList $ map (\x -> TImp x b) $ toList xs

--   -- TImp (TAnd xs) b -> Just $ TOr $ HashSet.fromList $ map (\x -> TImp x b) $ toList xs

--   TImp (TOr xs) b
--     | all isAnd xs
--     , and [ all isPred ys | TAnd ys <- toList xs]
--     -> case b of
--       TAnd ys | all isPred ys -> Just $ TOr $ HashSet.fromList [TAnd $ HashSet.union x ys | TAnd x <- toList xs]
--       TPred y -> Just $ TOr $ HashSet.fromList [TAnd $ HashSet.union x (HashSet.singleton (TPred y)) | TAnd x <- toList xs]
--       TOr zs | all isAnd zs, and [all isPred qs | TAnd qs <- toList zs] 
--         -> Just $ TOr $ HashSet.fromList [TAnd $ HashSet.union ys qs | TAnd ys <- toList xs, TAnd qs <- toList zs]

--   TImp (TOr xs) b -> Just $ TAnd $ HashSet.fromList $ map (\x -> TImp x b) $ toList xs


--   TImp (TAnd xs) (TOr ys) | all isPred xs, all isAnd ys, and [all isPred zs | TAnd zs <- toList ys]
--     -> Just $ TOr $ HashSet.fromList [TAnd $ HashSet.union y xs | TAnd y <- toList ys]
--   TImp (TAnd xs) (TAnd ys) | all isPred xs, all isPred ys -> Just $ TAnd $ HashSet.union xs ys
--   TImp (TAnd xs) (TPred y) | all isPred xs -> Just $ TAnd $ HashSet.union xs $ HashSet.singleton (TPred y)
--   TImp (TPred y) (TAnd xs) | all isPred xs -> Just $ TAnd $ HashSet.union xs $ HashSet.singleton (TPred y)
--   TImp (TPred x) (TPred y) -> Just $ TAnd $ HashSet.fromList [TPred x, TPred y]
--   TImp (TPred x) (TOr ys) | all isAnd ys, and [all isPred zs | TAnd zs <- toList ys]
--     -> Just $ TOr $ HashSet.fromList [TAnd $ HashSet.union y $ HashSet.singleton (TPred x) | TAnd y <- toList ys]

--   -- TImp a b -> Just $ TAnd $ HashSet.fromList [a,b]
--   TImp a b -> Just $ TOr $ HashSet.fromList [neg a, b]
--   -- TImp a b -> Just $ TOr $ HashSet.fromList [neg a, TAnd $ HashSet.fromList [a,b]]
--   -- TImp a b -> Just $ TOr $ HashSet.fromList 
--   --                        [ TAnd $ HashSet.fromList [neg a, neg b]
--   --                        , TAnd $ HashSet.fromList [neg a,     b]
--   --                        , TAnd $ HashSet.fromList [    a,     b]]

--   -- TIff a b -> Just $ TAnd $ HashSet.fromList [TImp a b, TImp b a]
--   TIff a b -> Just $ TOr $ HashSet.fromList $ 
--                          [ TAnd $ HashSet.fromList [neg a, neg b]
--                          , TAnd $ HashSet.fromList [a,b]
--                          ]
  
--   -- TNeg (TPred (TRel r e1 e2)) -> Just $ TPred (TRel (invRel r) e1 e2)

--   -- a | isUnsolvable a -> Just TFalse

--   TPred p -> evalP p

--   TAll x b t -> Just $ runST $ do
--     let Name x' _ = x
--     -- unless (isDummy x) $ do
--     --   traceM $ replicate 40 '-'
--     --   traceM $ showPretty {-$ traceGraph ("trace_" ++ showPretty x' ++ "_1.svg")-} $ TAll x b t
--     --   traceM "⇝"
--     let t' = mkDisjuncts $ map (varElim x b) $ toPreds t
--     -- unless (isDummy x) $ do
--     --   traceM $ showPretty {-$ traceGraph ("trace_" ++ showPretty x' ++ "_2.svg")-} t'
--     return t'
  
--   _ -> Nothing

isUnsolvable :: Tree -> Bool
isUnsolvable t = or [isBot a | TAbs a <- Uniplate.universeBi t]
  where
    isBot (ABool   a) = a == (⊥)
    isBot (AInt    a) = a == (⊥)
    isBot (AString a) = a == (⊥)


-- TODO: allow more expression meets
instance PartialMeetSemilattice PExpr where  
  PAbs a ∧? PAbs b = PAbs <$> a ∧? b
  
   -- TODO: Is this correct?
  PAbs (ABool   a) ∧? e | a == (⊤) = Just e
  PAbs (AInt    a) ∧? e | a == (⊤) = Just e
  PAbs (AString a) ∧? e | a == (⊤) = Just e

  a ∧? b | a == b    = Just a
         | otherwise = Nothing


-- | Algorithm 3 in OOPSLA'23 submission.
varElim :: Name -> Base -> [Pred] -> [Pred]
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
substExpr :: PExpr -> Name -> Pred -> Pred
substExpr x̂ x p = case p of
  -- TReg e re -> TReg (go e) re
  PRel r e1 e2 -> PRel r (go e1) (go e2)
 where
  go = \case
    PVal (Var y) | y == x -> x̂
    PVal (Var y) -> PVal (Var y)
    PVal (Con c) -> PVal (Con c)
    PAbs a -> PAbs a
    PAdd e1 e2 -> PAdd (go e1) (go e2)
    PSub e1 e2 -> PSub (go e1) (go e2)
    PStrLen e -> PStrLen (go e)
    PStrAt e1 e2 -> PStrAt (go e1) (go e2)
    PNot2 e -> PNot2 (go e)    

evalP :: Pred -> Maybe Pred
-- evalP (TReg e re) = case evalE e of
--   TCon (S s _)     -> Just $ undefined s re -- TODO: regex inclusion
--   PAbs (AString s) -> Just $ undefined s re -- TODO: abstract regex inclusion?
--   e' | e' /= e     -> Just $ TPred $ TReg e' re
--   _                -> Nothing

evalP (PRel r e1 e2) = case (evalE e1, evalE e2) of

  (PVar x, PCon (B b pv)) -> case r of
    Ne -> Just $ PRel Eq (PVar x) (PCon (B (not b) pv))
    _ -> Nothing

  (PNot2 x@(PVar _), e2'@(PCon (B _ _))) -> Just $ PRel (invRel r) x e2'

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
    | e1' /= e1 || e2' /= e2 -> Just $ PRel r e1' e2'
    | otherwise              -> Nothing

evalE :: PExpr -> PExpr
evalE = \case
  PNot2 (PCon (B b pv)) -> PCon (B (not b) pv)
  PNot2 (PAbs a) -> evalE $ PAbs (neg a)
  e -> e

