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



-- | Rewriting from § 4.1 in OOPSLA'23 submission.
rewrite :: Tree -> Tree
rewrite = Uniplate.rewrite $ \case
  -- TAnd TFalse _ -> Just TFalse
  -- TAnd _ TFalse -> Just TFalse
  -- TAnd a TTrue -> Just a
  -- TAnd TTrue a -> Just a
  -- TAnd a b | a == b -> Just a

  -- TOr _ TTrue -> Just TTrue
  -- TOr TTrue _ -> Just TTrue
  -- TOr TFalse a -> Just a
  -- TOr a TFalse -> Just a
  -- TOr a b | a == b -> Just a

  TNeg TTrue -> Just TFalse
  TNeg TFalse -> Just TTrue

  TOr xs | any isOr xs -> Just $ foldr tOr TFalse xs
  
  -- TODO: hack; solve this normalization more generally
  TAnd xs | TTrue `elem` xs -> Just $ foldr tAnd TTrue xs

  TAnd xs | any isOr xs -> Just $ runST $ do
    --traceM $ showPretty $ TAnd xs
    let (TOr ys : yys, zs) = partition isOr $ toList xs
    let cs = [foldr tAnd TTrue $ y : (yys ++ zs) | y <- toList ys]
    let ds = foldr tOr TFalse cs
    --traceM $ "⇝" ++ showPretty cs ++ "\n"
    return ds
    --error "HEY"
  --   return $ TAnd xs
      --let blah = [z `tAnd` x' | y@(TOr zs) <- ys, z <- toList zs, x' <- xs, x' /= y]
      --traceM $ showPretty blah
      --return $ foldr tOr TFalse blah

  -- TAnd (toList -> [a, TOr (toList -> [b,c])]) -> Just $ (a `tAnd` b) `tOr` (a `tAnd` c)
  -- TAnd (toList -> [TOr (toList -> [a,b]), c]) -> Just $ (a `tAnd` c) `tOr` (b `tAnd` c)

  --TAnd (toList -> xs) | any isOr xs ->    
    --Just $ foldr tOr TFalse [x `tAnd` xs' | x@(TOr _) <- xs, let xs' = foldr tAnd TTrue $ filter (/= x) xs]

  -- a `TAnd` (b `TOr` c) -> Just $ (a `tAnd` b) `tOr` (a `tAnd` c)
  -- (a `TOr` b) `TAnd` c -> Just $ (a `tAnd` c) `tOr` (b `tAnd` c)

  TNeg (TNeg a) -> Just a
  TNeg (TAnd xs) -> Just $ foldr tOr TFalse $ map TNeg $ toList xs
  TNeg (TOr  xs) -> Just $ foldr tAnd TTrue $ map TNeg $ toList xs
  -- TNeg (a `TAnd` b) -> Just $ (TNeg a) `tOr` (TNeg b)
  -- TNeg (a `TOr` b) -> Just $ (TNeg a) `tAnd` (TNeg b)

  a `TImp` b -> Just $ (TNeg a) `tOr` (a `tAnd` b)
  a `TIff` b -> Just $ ((TNeg a) `tAnd` (TNeg b)) `tOr` (a `tAnd` b)

  TNeg (TPred (TRel r e1 e2)) -> Just $ TPred (TRel (invRel r) e1 e2)

  a | isUnsolvable a -> Just TFalse

  -- TODO: is this the right place?
  TPred p -> evalP p

  TAll x b t -> Just $ runST $ do
    let Name x' _ = x
    -- unless (isDummy x) $ do
    --   traceM $ replicate 40 '-'
    --   traceM $ showPretty $ traceGraph ("trace_" ++ showPretty x' ++ "_1.svg") $ TAll x b t
    --   traceM "⇝"
    let t' = mkDisjuncts $ map (varElim x b) $ toPreds t
    -- unless (isDummy x) $ do
    --   traceM $ showPretty $ traceGraph ("trace_" ++ showPretty x' ++ "_2.svg") t'
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
  x̂sRef <- newSTRef (Map.empty :: Map.Map [Name] TExpr)
  
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

