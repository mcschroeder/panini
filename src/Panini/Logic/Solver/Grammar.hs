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
import Panini.Logic.Expressions
import Panini.Logic.Predicates
import Panini.Logic.Relations
import Panini.Names
import Panini.Pretty.Printer
import Panini.Primitives
import Panini.Substitution
import Prelude
import Data.Text qualified as Text

-------------------------------------------------------------------------------

infer :: Name -> Con -> Pred
infer s = PRel . concretizeVar s . EAbs . AString
        . joins
        . map (meets . map (abstractStringVar s))  -- TODO
        -- . unwrapDNF
        . unDNF . rewrite4

-- TODO: either make this unnecessary or deal with errors gracefully
abstractStringVar :: Name -> Rel -> AString
abstractStringVar x p = case abstractVar x TString p of
  EAbs (AString s) -> s
  _                -> error "expected abstract string"

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

showPretty' = Text.unpack . renderDoc (RenderOptions (Just defaultStyling) True (Just 80)) . pretty

-- TODO: cleanup rewrite rewrite to make more sense

rewrite :: Con -> [[Rel]]  -- DNF?
rewrite c0 = runST $ do
  traceM $ replicate 40 '-' ++ "\nINPUT:\n" ++ showPretty c0
  let logReturn x = traceM ("OUTPUT:\n" ++ showPretty x ++ "\n" ++ replicate 40 '-') >> return x
  case c0 of
    CHead p -> logReturn $ toDNF p
    CAnd c1 c2 -> do
      traceM $ showPretty' $ "REWRITE:" <\> "⋀" <+> nest 2 (pretty [c1,c2])
      logReturn $ toDNF $ PAnd [wrapDNF $ rewrite c1, wrapDNF $ rewrite c2]
    CAll x b p c -> do
      let ps = toDNF p
      traceM $ showPretty' $ "ANTECEDENTS:" <\> "⋁" <+> nest 2 (pretty ps)
      rs <- forM ps $ \p' -> case c of
        CAnd c1 c2 -> do
          let c1' = CAll x b (wrapDNF [p']) c1
          let c2' = CAll x b (wrapDNF [p']) c2
          traceM $ showPretty' $ "REWRITE:" <\> "⋀" <+> nest 2 (pretty [c1',c2'])
          case rewrite c1' of
            [] -> case rewrite c2' of
              [] -> logReturn []
              c2'' -> logReturn c2''
            c1'' -> case rewrite c2' of
              [] -> logReturn c1''
              c2'' -> logReturn $ toDNF $ PAnd [wrapDNF c1'', wrapDNF c2'']
        CHead q -> do
          logReturn $ mapMaybe (varElim x b) $ toDNF $ PAnd [wrapDNF [p'], q]
        CAll x2 b2 p2 c2 -> do
          let c' = CAll x2 b2 (wrapDNF $ toDNF $ PAnd [wrapDNF [p'],p2]) c2
          traceM $ showPretty' $ "REWRITE:" <\>  pretty (CAll x b PTrue c')
          logReturn $ mapMaybe (varElim x b) $ rewrite c'
      logReturn $ concat rs 


rewrite3 :: Con -> [[Rel]]  -- DNF?
rewrite3 = \case
  CHead p -> toDNF p
  CAnd c1 c2 -> toDNF $ PAnd [wrapDNF $ rewrite3 c1, wrapDNF $ rewrite3 c2]
  CAll x b p c -> concat $ flip map (toDNF p) $ \p' -> case c of
    CHead q -> mapMaybe (varElim x b) $ toDNF $ PAnd [wrapDNF [p'], q]
    CAnd c1 c2 ->
      let c1' = mapMaybe (varElim x b) $ toDNF $ PAnd [wrapDNF [p'], wrapDNF $ rewrite3 c1]
          c2' = mapMaybe (varElim x b) $ toDNF $ PAnd [wrapDNF [p'], wrapDNF $ rewrite3 c2]
      in case (c1', c2') of
        ([],[]) -> []
        ([],c2'') -> c2''
        (c1'',[]) -> c1''
        (c1'',c2'') -> toDNF $ PAnd [wrapDNF c1'', wrapDNF c2'']
    CAll x2 b2 p2 c2 ->
      let p2' = PAnd [wrapDNF [p'], p2]
      in mapMaybe (varElim x b) $ rewrite3 $ CAll x2 b2 p2' c2


rewrite4 :: Con -> DNF Rel
rewrite4 = \case
  CHead p -> toDNF2 p
  CAnd c1 c2 -> rewrite4 c1 ∧ rewrite4 c2
  CAll x b p c -> foldr (∨) (DNF []) $ flip map (splitDNF $ toDNF2 p) $ \p' -> case c of
    CHead q -> varElimDNF x b $ p' ∧ toDNF2 q
    CAnd c1 c2 ->
      let c1' = varElimDNF x b $ p' ∧ rewrite4 c1
          c2' = varElimDNF x b $ p' ∧ rewrite4 c2
      in case (c1', c2') of
        (DNF [], DNF []) -> DNF []
        (DNF [], c2''  ) -> c2''
        (c1'',   DNF []) -> c1''
        (c1'',   c2''  ) -> c1'' ∧ c2''
    CAll x2 b2 p2 c2 ->
      let p2' = wrapDNF $ unDNF $ p' ∧ toDNF2 p2      
      in varElimDNF x b $ rewrite4 $ CAll x2 b2 p2' c2

varElimDNF :: Name -> Base -> DNF Rel -> DNF Rel
varElimDNF x b = DNF . mapMaybe (varElim x b) . unDNF

unDNF :: DNF a -> [[a]]
unDNF (DNF ps) = ps

splitDNF :: DNF a -> [DNF a]
splitDNF (DNF ps) = map (DNF . pure) ps

newtype DNF a = DNF [[a]]

instance Semigroup (DNF a) where
  (<>) = (∨)

instance Monoid (DNF a) where
  mempty = DNF []

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


-- rewrite3 :: Con -> [[Rel]]  -- DNF?
-- rewrite3 c0 = do
--   case c0 of
--     CHead p -> toDNF p
--     CAnd c1 c2 -> do
--       toDNF $ PAnd [wrapDNF $ rewrite3 c1, wrapDNF $ rewrite3 c2]
--     CAll x b p c ->
--       concat $ flip map (toDNF p) $ \p' -> case c of
--               CAnd c1 c2 ->
--                 let c1' = CAll x b (wrapDNF [p']) c1
--                     c2' = CAll x b (wrapDNF [p']) c2
--                 in case (rewrite3 c1', rewrite3 c2') of
--                     ([],[]) -> []
--                     ([],c2'') -> c2''
--                     (c1'',[]) -> c1''
--                     (c1'',c2'') -> toDNF $ PAnd [wrapDNF c1'', wrapDNF c2'']
--               CHead q ->
--                 mapMaybe (varElim x b) $ toDNF $ PAnd [wrapDNF [p'], q]
--               CAll x2 b2 p2 c2 ->
--                 let c' = CAll x2 b2 (wrapDNF $ toDNF $ PAnd [wrapDNF [p'],p2]) c2
--                 in mapMaybe (varElim x b) $ rewrite3 c'

  -- CHead p -> toDNF p
  -- CAnd c1 c2 -> toDNF $ PAnd [wrapDNF $ rewrite c1, wrapDNF $ rewrite c2]
  -- CAll _ _ PFalse _ -> unwrapDNF PTrue
  -- CAll x b (POr ps) c ->
  --   toDNF $ POr $ map (\p' -> wrapDNF $ rewrite $ CAll x b p' c) ps
  -- CAll x b p (CAnd c1 c2) ->
  --   toDNF $ PAnd [wrapDNF $ rewrite $ CAll x b p c1, wrapDNF $ rewrite $ CAll x b p c2]
  -- CAll x b p1 (CHead p2) ->
  --   mapMaybe (varElim x b) $ toDNF $ PAnd [p1,p2]
  -- CAll x1 b1 p1 (CAll x2 b2 p2 c2) ->
  --   mapMaybe (varElim x1 b1) $ rewrite $ CAll x2 b2 (wrapDNF $ toDNF $ PAnd [p1,p2]) c2

-------------------------------------------------------------------------------

toDNF2 :: Pred -> DNF Rel
toDNF2 = DNF . unwrapDNF . toDNF'

toDNF :: Pred -> [[Rel]]
toDNF = unwrapDNF . toDNF'

toDNF' :: Pred -> Pred
toDNF' = Uniplate.rewrite $ \case
  PAnd xs
    | any (== PFalse) xs -> Just PFalse
    | any (== PTrue) xs -> Just $ PAnd $ List.filter (/= PTrue) xs
    | any isPAnd xs ->
      let (ys, zs) = partition isPAnd xs
      in Just $ PAnd $ mconcat [y | PAnd y <- ys] ++ zs
    | any isPOr xs -> case partition isPOr xs of
        (POr ys : yys, zs) -> Just $ POr $ [PAnd $ y : (yys ++ zs) | y <- ys]
        _ -> error "impossible"
    -- | or [neg x `elem` xs | x <- xs] -> Just PFalse -- TODO: this needs a normal form / eval
    | HashSet.size (HashSet.fromList xs) < length xs -> Just $ PAnd $ List.nub xs  -- TODO
    | [x] <- xs -> Just x

  POr xs
    | any (== PTrue) xs -> Just PTrue
    | any (== PFalse) xs -> Just $ POr $ List.filter (/= PFalse) xs
    | any isPOr xs ->
      let (ys, zs) = partition isPOr xs
      in Just $ POr $ mconcat [y | POr y <- ys] ++ zs
    | [x] <- xs -> Just x

  PNot (POr xs) -> Just $ PAnd $ map neg xs
  PNot (PAnd xs) -> Just $ POr $ map neg xs

  PImpl a b -> Just $ POr [neg a, b]

  PIff a b -> Just $ POr [ PAnd [neg a, neg b], PAnd [a, b]]

  -- PNot (PRel r) -> Just $ PRel $ inverse r

  -- p@(PRel _) -> evalP p

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

wrapDNF :: [[Rel]] -> Pred
wrapDNF xs = case POr $ map (PAnd . map PRel) xs of
  POr []        -> PFalse
  POr [PAnd []] -> PTrue
  p             -> p

-------------------------------------------------------------------------------

-- varElimDNF :: Name -> Base -> Pred -> Pred
-- varElimDNF x b = wrapDNF . map (varElim x b) . unwrapDNF

-- TODO: cleanup rewrite in varElim due to bottom meets not being handled

-- | Algorithm 3 in OOPSLA'23 submission.
varElim :: Name -> Base -> [Rel] -> Maybe [Rel]
varElim _ TUnit ps = Just ps
varElim x b ps = runST $ do

  let logTrace str = traceM $ showPretty $ "varElim" <+> pretty x <+> pretty b <+> str

  logTrace $ "INPUT:" <+> pretty ps

  let bTop = topExpr b
  x̂sRef <- newSTRef $ Map.singleton [x] bTop

  forM_ [(p,v̄) | p <- ps, let v̄ = freeVars p, x `elem` v̄] $ \(p,v̄) -> do
    x̂₀ <- fromMaybe bTop . Map.lookup v̄ <$> readSTRef x̂sRef
    let x̂₁ = abstractVar x b p
    case x̂₀ ∧? x̂₁ of
      Just x̂ -> modifySTRef' x̂sRef $ Map.insert v̄ x̂
      Nothing -> error $ "cannot meet " ++ showPretty x̂₀ ++ " with " ++ showPretty x̂₁

  x̂Self <- fromJust . Map.lookup [x] <$> readSTRef x̂sRef
  logTrace $ "self meet:" <+> pretty x̂Self
  case x̂Self of
    EAbs (ABool   a) | isBot a -> return Nothing
    EAbs (AInt    a) | isBot a -> return Nothing
    EAbs (AString a) | isBot a -> return Nothing
    _ -> do
      x̂s' <- filter (([x] /=) . fst) . Map.assocs <$> readSTRef x̂sRef
      (v̄ₘ,x̂ₘ) <- if null x̂s'
        then do
          -- x̂Self <- fromJust . Map.lookup [x] <$> readSTRef x̂sRef
          pure ([x], x̂Self)
        else
          pure $ head x̂s'  -- TODO: pick "smallest" meet

      logTrace $ "reciprocal meet:" <+> pretty (v̄ₘ,x̂ₘ)

      let qs = map (substExpr x̂ₘ x) $ filter ((v̄ₘ /=) . freeVars) ps

      logTrace $ "OUTPUT:" <+> pretty qs

      return $ Just qs

-- TODO: integrate into existing substitution architecture
substExpr :: Expr -> Name -> Rel -> Rel
substExpr x̂ x p = case p of
  Rel r e1 e2 -> Rel r (go e1) (go e2)
 where
  go = \case
    EVal (Var y) | y == x -> x̂
    EVal (Var y) -> EVal (Var y)
    EVal (Con c) -> EVal (Con c)
    EAbs a -> EAbs a
    EAdd e1 e2 -> EAdd (go e1) (go e2)
    EMul e1 e2 -> EMul (go e1) (go e2)
    ESub e1 e2 -> ESub (go e1) (go e2)
    EStrLen e -> EStrLen (go e)
    EStrAt e1 e2 -> EStrAt (go e1) (go e2)
    EStrSub e1 e2 e3 -> EStrSub (go e1) (go e2) (go e3)
    EFun f xs -> EFun f (map go xs)
    ENot e -> ENot (go e)

-------------------------------------------------------------------------------

-- TODO: do we want this?
instance ComplementedLattice Pred where
  neg (PRel r@(Rel _ _ _)) =
    let p' = PRel $ inverse r
    in case evalP p' of
      Nothing -> p'
      Just p'' -> p''
  neg (PNot p) = p
  neg p = PNot p


evalP :: Pred -> Maybe Pred
-- evalP (TReg e re) = case evalE e of
--   TCon (S s _)     -> Just $ undefined s re -- TODO: regex inclusion
--   EAbs (AString s) -> Just $ undefined s re -- TODO: abstract regex inclusion?
--   e' | e' /= e     -> Just $ TPred $ TReg e' re
--   _                -> Nothing

evalP (PRel (Rel r e1 e2)) = case (evalE e1, evalE e2) of

  (EVar x, ECon (B b pv)) -> case r of
    Ne -> Just $ PRel $ Rel Eq (EVar x) (ECon (B (not b) pv))
    _ -> Nothing

  (ENot x@(EVar _), (ECon (B b pv))) -> case r of
    Eq -> Just $ PRel $ Rel Eq x (ECon (B (not b) pv))
    Ne -> Just $ PRel $ Rel Eq x (ECon (B b pv))
    _ -> Nothing

  (ECon (B b1 _), ECon (B b2 _)) -> case r of
    Eq | b1 == b2 -> Just PTrue
    Ne | b1 /= b2 -> Just PTrue
    _             -> Just PFalse  -- TODO: might be hiding type error case

    -- TODO: comparison with abstract values is an abstract operation?
    -- {T,F} = T  ??? true or false?

  -- TODO: hardcoded hack; replace with general eval
  (EAbs (ABool b1), ECon (B b2 _)) -> case concreteBool b1 of
    Nothing  -> Nothing
    Just b1' -> case r of
      Eq | b1' == b2 -> Just PTrue
      Ne | b1' /= b2 -> Just PTrue
      _              -> Just PFalse  -- TODO: might be hiding type error case

  (ECon (I i1 _), ECon (I i2 _)) -> case r of
    Eq | i1 == i2 -> Just PTrue
    Ne | i1 /= i2 -> Just PTrue
    Gt | i1 >  i2 -> Just PTrue
    Ge | i1 >= i2 -> Just PTrue
    Lt | i1 <  i2 -> Just PTrue
    Le | i1 <= i2 -> Just PTrue
    _             -> Just PFalse  -- TODO: might be hiding type error case

  (ECon (S s1 _), ECon (S s2 _)) -> case r of
    Eq | s1 == s2 -> Just PTrue
    Ne | s1 /= s2 -> Just PTrue
    _             -> Just PFalse  -- TODO: might be hiding type error case

  (e1', e2')
    | e1' /= e1 || e2' /= e2 -> Just $ PRel $ Rel r e1' e2'
    | otherwise              -> Nothing

evalP _ = Nothing

evalE :: Expr -> Expr
evalE = \case
  ENot (ECon (B b pv)) -> ECon (B (not b) pv)
  ENot (EAbs (ABool a)) -> evalE $ EAbs (ABool $ neg a)
  e -> e

-------------------------------------------------------------------------------

isPOr :: Pred -> Bool
isPOr (POr _) = True
isPOr _       = False

isPAnd :: Pred -> Bool
isPAnd (PAnd _) = True
isPAnd _        = False

isPRel :: Pred -> Bool
isPRel (PRel (Rel _ _ _)) = True
isPRel _                  = False