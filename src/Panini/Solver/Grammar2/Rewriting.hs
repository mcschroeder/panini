module Panini.Solver.Grammar2.Rewriting where

import Data.Generics.Uniplate.Operations as Uniplate
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

-- TODO: new new idea: interpret Con directly:
-- ⟦ CAnd c1 c2 ⟧   = ⟦ c1 ⟧ + ⟦ c2 ⟧
-- ⟦ CHead p ⟧ = interpret p abstractly (∧/∨ are abstract value meet/join) 
-- ⟦ CAll x b p c ⟧ = ⟦ p ⟧ × ⟦ c ⟧  (merge/meet together) and then resolve x



-- | Rewriting from § 4.1 in OOPSLA'23 submission.
rewrite :: Tree -> Tree
rewrite = (endtrace .) $ Uniplate.rewrite $ tracer $ \case  
  TAnd xs
    | any (== TFalse) xs -> Just TFalse
    | any (== TTrue) xs -> Just $ TAnd $ HashSet.delete TTrue xs
    | any isAnd xs ->
      let (ys, zs) = partition isAnd $ toList xs
          ys' = HashSet.unions [y | TAnd y <- ys]
          zs' = HashSet.fromList zs
      in Just $ TAnd $ HashSet.union ys' zs'
    | any isOr xs ->
      let (TOr ys : yys, zs) = partition isOr $ toList xs
          cs = [TAnd $ HashSet.fromList $ y : (yys ++ zs) | y <- toList ys]
      in Just $ TOr $ HashSet.fromList cs
    | or [neg x `HashSet.member` xs | x <- toList xs] -> Just TFalse
    | [x] <- toList xs -> Just x
    
  TOr xs
    | any (== TTrue) xs -> Just TTrue
    | any (== TFalse) xs -> Just $ TOr $ HashSet.delete TFalse xs
    | any isOr xs -> 
      let (ys, zs) = partition isOr $ toList xs
          ys' = HashSet.unions [y | TOr y <- ys]
          zs' = HashSet.fromList zs
      in Just $ TOr $ HashSet.union ys' zs'
    | [x] <- toList xs -> Just x
    
    -- TODO: | merge subsets
  
  TNeg (TPred p) -> Just $ TPred (neg p)
  TNeg (TOr xs) -> Just $ TAnd $ HashSet.map neg xs  
  -- TNeg (TAnd xs) -> 
  --     Just $ TOr $ HashSet.fromList
  --          $ map (TAnd . HashSet.fromList)
  --          $ tail
  --          $ traverse (\x -> [x, neg x]) 
  --          $ toList xs
  TNeg (TAnd xs) 
    | all isPred xs -> 
      Just $ TOr $ HashSet.fromList
           $ map (TAnd . HashSet.fromList)
           $ tail
           $ traverse (\x -> [TPred x, TPred $ neg x]) 
           $ [x | TPred x <- toList xs]
  TNeg (TAnd xs) -> Just $ TOr $ HashSet.map neg xs
  -- TNeg (TAnd xs) | all isPred xs -> Nothing
  -- TNeg (TAnd xs)
  --   | all isPred xs -> Just $ TAnd $ HashSet.map neg xs
  --   | otherwise     -> Just $ TOr  $ HashSet.map neg xs

  -- TImp (TOr xs) b 
  --   | all isAnd xs
  --   , and [ all isPred ys | TAnd ys <- toList xs] 
  --   -> Just $ TOr $ HashSet.fromList $ map (\x -> TImp x b) $ toList xs

  -- TImp (TAnd xs) b -> Just $ TOr $ HashSet.fromList $ map (\x -> TImp x b) $ toList xs

  TImp (TOr xs) b
    | all isAnd xs
    , and [ all isPred ys | TAnd ys <- toList xs]
    -> case b of
      TAnd ys | all isPred ys -> Just $ TOr $ HashSet.fromList [TAnd $ HashSet.union x ys | TAnd x <- toList xs]
      TPred y -> Just $ TOr $ HashSet.fromList [TAnd $ HashSet.union x (HashSet.singleton (TPred y)) | TAnd x <- toList xs]
      TOr zs | all isAnd zs, and [all isPred qs | TAnd qs <- toList zs] 
        -> Just $ TOr $ HashSet.fromList [TAnd $ HashSet.union ys qs | TAnd ys <- toList xs, TAnd qs <- toList zs]

  TImp (TOr xs) b -> Just $ TAnd $ HashSet.fromList $ map (\x -> TImp x b) $ toList xs


  TImp (TAnd xs) (TOr ys) | all isPred xs, all isAnd ys, and [all isPred zs | TAnd zs <- toList ys]
    -> Just $ TOr $ HashSet.fromList [TAnd $ HashSet.union y xs | TAnd y <- toList ys]
  TImp (TAnd xs) (TAnd ys) | all isPred xs, all isPred ys -> Just $ TAnd $ HashSet.union xs ys
  TImp (TAnd xs) (TPred y) | all isPred xs -> Just $ TAnd $ HashSet.union xs $ HashSet.singleton (TPred y)
  TImp (TPred y) (TAnd xs) | all isPred xs -> Just $ TAnd $ HashSet.union xs $ HashSet.singleton (TPred y)
  TImp (TPred x) (TPred y) -> Just $ TAnd $ HashSet.fromList [TPred x, TPred y]
  TImp (TPred x) (TOr ys) | all isAnd ys, and [all isPred zs | TAnd zs <- toList ys]
    -> Just $ TOr $ HashSet.fromList [TAnd $ HashSet.union y $ HashSet.singleton (TPred x) | TAnd y <- toList ys]

  -- TImp a b -> Just $ TAnd $ HashSet.fromList [a,b]
  TImp a b -> Just $ TOr $ HashSet.fromList [neg a, b]
  -- TImp a b -> Just $ TOr $ HashSet.fromList [neg a, TAnd $ HashSet.fromList [a,b]]
  -- TImp a b -> Just $ TOr $ HashSet.fromList 
  --                        [ TAnd $ HashSet.fromList [neg a, neg b]
  --                        , TAnd $ HashSet.fromList [neg a,     b]
  --                        , TAnd $ HashSet.fromList [    a,     b]]

  -- TIff a b -> Just $ TAnd $ HashSet.fromList [TImp a b, TImp b a]
  TIff a b -> Just $ TOr $ HashSet.fromList $ 
                         [ TAnd $ HashSet.fromList [neg a, neg b]
                         , TAnd $ HashSet.fromList [a,b]
                         ]
  
  -- TNeg (TPred (TRel r e1 e2)) -> Just $ TPred (TRel (invRel r) e1 e2)

  -- a | isUnsolvable a -> Just TFalse

  TPred p -> evalP p

  TAll x b t -> Just $ runST $ do
    let Name x' _ = x
    -- unless (isDummy x) $ do
    --   traceM $ replicate 40 '-'
    --   traceM $ showPretty {-$ traceGraph ("trace_" ++ showPretty x' ++ "_1.svg")-} $ TAll x b t
    --   traceM "⇝"
    let t' = mkDisjuncts $ map (varElim x b) $ toPreds t
    -- unless (isDummy x) $ do
    --   traceM $ showPretty {-$ traceGraph ("trace_" ++ showPretty x' ++ "_2.svg")-} t'
    return t'
  
  _ -> Nothing

isUnsolvable :: Tree -> Bool
isUnsolvable t = or [isBot a | TAbs a <- universeBi t]
  where
    isBot (ABool   a) = a == (⊥)
    isBot (AInt    a) = a == (⊥)
    isBot (AString a) = a == (⊥)


-- | Algorithm 3 in OOPSLA'23 submission.
varElim :: Name -> Base -> [TPred] -> [TPred]
varElim x b ps = runST $ do

  -- traceM $ "varElim " ++ showPretty x ++ " " ++ showPretty b ++ " " ++ showPretty ps

  let bTop = topExpr b
  x̂sRef <- newSTRef $ Map.empty
  
  forM_ [(p,v̄) | p <- ps, let v̄ = varsP p, x `elem` v̄] $ \(p,v̄) -> do
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

  let qs = map (substExpr x̂ₘ x) $ filter ((v̄ₘ /=) . varsP) ps

  -- traceM $ "  " ++ showPretty qs

  return qs


substExpr :: TExpr -> Name -> TPred -> TPred
substExpr x̂ x p = case p of
  TReg e re -> TReg (go e) re
  TRel r e1 e2 -> TRel r (go e1) (go e2)
 where
  go = \case
    TVal (Var y) | y == x -> x̂
    TVal (Var y) -> TVal (Var y)
    TVal (Con c) -> TVal (Con c)
    TAbs a -> TAbs a
    TAdd e1 e2 -> TAdd (go e1) (go e2)
    TSub e1 e2 -> TSub (go e1) (go e2)
    TStrLen e -> TStrLen (go e)
    TStrAt e1 e2 -> TStrAt (go e1) (go e2)
    TNot e -> TNot (go e)    

evalP :: TPred -> Maybe Tree
evalP (TReg e re) = case evalE e of
  TCon (S s _)     -> Just $ undefined s re -- TODO: regex inclusion
  TAbs (AString s) -> Just $ undefined s re -- TODO: abstract regex inclusion?
  e' | e' /= e     -> Just $ TPred $ TReg e' re
  _                -> Nothing

evalP (TRel r e1 e2) = case (evalE e1, evalE e2) of

  (TNot x@(TVar _), e2'@(TCon (B _ _))) -> Just $ TPred $ TRel (invRel r) x e2'

  (TCon (B b1 _), TCon (B b2 _)) -> case r of
    Eq | b1 == b2 -> Just TTrue
    Ne | b1 /= b2 -> Just TTrue
    _             -> Just TFalse  -- TODO: might be hiding type error case
  
    -- TODO: comparison with abstract values is an abstract operation?
    -- {T,F} = T  ??? true or false?

  -- TODO: hardcoded hack; replace with general eval
  (TAbs (ABool b1), TCon (B b2 _)) -> case concreteBool b1 of
    Nothing  -> Nothing
    Just b1' -> case r of
      Eq | b1' == b2 -> Just TTrue
      Ne | b1' /= b2 -> Just TTrue
      _              -> Just TFalse  -- TODO: might be hiding type error case
    
  (TCon (I i1 _), TCon (I i2 _)) -> case r of
    Eq | i1 == i2 -> Just TTrue
    Ne | i1 /= i2 -> Just TTrue
    Gt | i1 >  i2 -> Just TTrue
    Ge | i1 >= i2 -> Just TTrue
    Lt | i1 <  i2 -> Just TTrue
    Le | i1 <= i2 -> Just TTrue
    _             -> Just TFalse  -- TODO: might be hiding type error case
  
  (TCon (S s1 _), TCon (S s2 _)) -> case r of
    Eq | s1 == s2 -> Just TTrue
    Ne | s1 /= s2 -> Just TTrue
    _             -> Just TFalse  -- TODO: might be hiding type error case
  
  (e1', e2') 
    | e1' /= e1 || e2' /= e2 -> Just $ TPred $ TRel r e1' e2'
    | otherwise              -> Nothing

evalE :: TExpr -> TExpr
evalE = \case
  TNot (TCon (B b pv)) -> TCon (B (not b) pv)
  TNot (TAbs a) -> evalE $ TAbs (neg a)
  e -> e

