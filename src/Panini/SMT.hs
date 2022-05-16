{-# LANGUAGE OverloadedStrings #-}

module Panini.SMT
  ( printSMTLib2
  , solve
  , sat
  ) where

import Control.Monad
import Data.List (partition, dropWhileEnd, nub)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.String
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder qualified as LB
import Panini.Provenance
import Panini.Printer
import Panini.Substitution
import Panini.Syntax
import Prelude
import System.Process
import Data.Char
import System.Exit

------------------------------------------------------------------------------

outputPretty :: Pretty a => a -> IO ()
outputPretty x = do
  let opts = RenderOptions {unicodeSymbols = True, ansiColors = True, fixedWidth = Nothing}
  putStrLn $ Text.unpack $ renderDoc opts $ pretty x

------------------

class Simplifable a where
  simplify :: a -> a

instance Simplifable Pred where
  -- TODO: preserve provenance information
  simplify = \case
    PAnd (simplifyAnd -> ps)
      | []  <- ps -> PTrue NoPV
      | [p] <- ps -> p
      | otherwise -> PAnd ps
    
    PDisj (simplify -> p) (simplify -> q)
      | PFalse _ <- p -> q
      | PFalse _ <- q -> p
      | otherwise     -> PDisj p q

    PNot (simplify -> p) -> PNot p

    PImpl (simplify -> p) (simplify -> q)
      | PTrue _ <- p -> q
      | PTrue _ <- q -> PTrue NoPV
      | otherwise    -> PImpl p q
  
    PIff (simplify -> p) (simplify -> q)
      | PTrue _ <- p -> q
      | PTrue _ <- q -> p
      | otherwise    -> PIff p q

    PExists x b (simplify -> p)
      | x `elem` freeVars p -> PExists x b p
      | otherwise           -> p
  
    PBin o (simplify -> p1) (simplify -> p2) -> PBin o p1 p2
    PRel r (simplify -> p1) (simplify -> p2) -> PRel r p1 p2
    PFun f (map simplify -> ps)              -> PFun f ps

    p -> p

instance Simplifable Con where
  simplify = \case
    CHead (simplify -> p) -> CHead p
    CAnd (simplify -> c1) (simplify -> c2)
      | CHead (PTrue _) <- c1 -> c2
      | CHead (PTrue _) <- c2 -> c1
      | otherwise     -> CAnd c1 c2
    CAll x b (simplify -> p) (simplify -> c)
      | x `elem` (freeVars p ++ freeVars c) -> CAll x b p c
      | PTrue _         <- p -> c
      | CHead (PTrue _) <- c -> CHead (PTrue NoPV)
      | otherwise            -> CAll x b p c

simplifyAnd :: [Pred] -> [Pred]
simplifyAnd = go []
  where
    go qs [] = reverse qs
    go qs ((simplify -> p) : ps)
      | PFalse _ <- p = [p]
      | taut p        = go qs ps
      | otherwise     = go (p:qs) ps

taut :: Pred -> Bool
taut (PTrue _)      = True
taut (PAnd [])      = True
taut (PAnd ps)      = all taut ps
taut (PRel Eq  p q) = p == q
taut (PRel Leq p q) = p == q
taut (PRel Geq p q) = p == q
taut _              = False

------------------

-- k is a set of refinement variables that cuts c
-- TODO: compute k
-- elim computes *exact* solutions for non-cut variables
-- solve computes approximate solutions for the remaining cut variables
-- (using predicate abstraction? from the tutorial (more or less?))

sat :: Con -> [Pred] -> IO Bool
sat c q = do
  let ks = [("k0", ["z1"]),("k1", ["z1"]), ("k2", ["z1"])] -- TODO
  let c' = simplify $ elim ks c
  putStrLn "---"
  outputPretty c'
  putStrLn "---"
  r <- solve c' q
  case r of
    Just s -> do
      forM_ (Map.toList s) $ \(k,(xs,p)) -> do
        outputPretty $ (PRel Eq (PHorn k (map V xs)) p)
      return True
    Nothing -> return False

type K = (Name,[Name])

elim :: [K] -> Con -> Con
elim []     c = c
elim (k:ks) c = elim ks (elim1 k c)

elim1 :: K -> Con -> Con
elim1 k c = elim' sk c
  where
    sk = Map.singleton (fst k) (snd k, sol1 k c')
    c' = flatHead2 $ scope k c

flatHead2 :: Con -> Con
flatHead2 (CAll _ _ _ c) = flatHead2 c
flatHead2 c = c

varnames :: [Value] -> [Name]
varnames = map go
  where
    go (V x) = x
    go _ = error "expected all xs in k(xs) to be variables"


sol1 :: K -> Con -> Pred
sol1 k (CAnd c1 c2)   = (sol1 k c1) `pOr` (sol1 k c2)
sol1 k (CAll x b p c) = PExists x b (p `pAnd` sol1 k c)
sol1 k (CHead (PHorn k2 ys))
  | fst k == k2       = PAnd $ map (\(x,y) -> pVar x `pEq` pVar y) 
                             $ zip (snd k) (varnames ys)
sol1 _ _              = PFalse NoPV

scope :: K -> Con -> Con
scope k (CAnd c1 c2)
  | k `elem` kvars c1, k `notElem` kvars c2 = scope k c1
  | k `notElem` kvars c1, k `elem` kvars c2 = scope k c2
scope k (CAll x b p c')
  | k `notElem` kvars p                     = CAll x b p (scope k c')
scope _ c                                   = c

elim' :: Assignment -> Con -> Con
elim' s (CAnd c1 c2)   = elim' s c1 `cAnd` elim' s c2
elim' s (CAll x b p c) = CAll x b (apply s p) (elim' s c)
elim' s (CHead (PHorn k _)) 
  | k `Map.member` s   = CTrue NoPV
elim' _ c              = c


class HasKVars a where
  kvars :: a -> [K]

instance HasKVars Pred where
  kvars = nub . go
    where
      go (PHorn k xs) = [(k, varnames xs)]  -- xs assumed to be just var names
      go (PBin _ p1 p2) = kvars p1 ++ kvars p2
      go (PRel _ p1 p2) = kvars p1 ++ kvars p2
      go (PAnd ps) = concatMap kvars ps
      go (PDisj p1 p2) = kvars p1 ++ kvars p2
      go (PImpl p1 p2) = kvars p1 ++ kvars p2
      go (PIff p1 p2) = kvars p1 ++ kvars p2
      go (PNot p) = kvars p
      go _ = []

instance HasKVars Con where
  kvars = nub . go
    where
      go (CHead p) = kvars p
      go (CAnd c1 c2) = kvars c1 ++ kvars c2
      go (CAll _ _ p c) = kvars p ++ kvars c

------------------------------------------------------------------------------
-- | Solve a Horn constraint given a set of candidates.
solve :: Con -> [Pred] -> IO (Maybe Assignment)
solve c _qs = do
  let cs = flat c
  putStrLn "---"
  mapM_ (putStrLn . showPretty) cs
  putStrLn "---"
  let (csk,csp) = partition horny cs
  let s0 = Map.empty
  s <- fixpoint csk s0
  r <- smtValid (map (applyCon s) csp)
  if r then return (Just s) else return Nothing

-- | Flatten a Horn constraint into a set of flat constraints each of which is
-- of the form ∀x1:b1. p1 ⇒ ∀x2:b2. p2 ⇒ ... ⇒ pn where pn is either a single
-- Horn application κ(ȳ) or a concrete predicate free of Horn variables.
flat :: Con -> [Con]
flat = split
  where
    split (CHead p)      = [CHead p]
    split (CAnd c1 c2)   = split c1 ++ split c2
    split (CAll x b p c) = [CAll x b p c' | c' <- split c]

-- | Whether or not a flat constraint has a Horn application in its head.
horny :: Con -> Bool
horny (CAll _ _ _ (CHead (PHorn _ _))) = True
horny _                                = False

-- | Iteratively weaken a candidate solution until an assignment satisfying all
-- given (flat) constraints is found.
fixpoint :: [Con] -> Assignment -> IO Assignment
fixpoint cs s = do  
  r <- take 1 <$> filterM ((not <$>) . smtValid . pure . applyCon s) cs
  case r of
    [c] -> do
      s' <- weaken s c
      fixpoint cs s'
    _ -> return s

-- | Weaken a Horn assignment to satisfy a given (flat) constraint.
weaken :: Assignment -> Con -> IO Assignment
weaken s c =
  case flatHead c of
    PHorn k xs -> case Map.lookup k s of
      Nothing -> error $ "expected Horn assignment for " ++ show k
      Just (ys,q0) -> do
        let c' = mapFlatBody (apply s) c
        let keep q = smtValid [mapFlatHead (const (substN xs ys q)) c']
        qs' <- PAnd <$> filterM keep (explode q0)
        return $ Map.insert k (ys,qs') s

    _ -> error "expected Horn variable at head of flat constraint"

explode :: Pred -> [Pred]
explode (PAnd ps) = ps
explode p = [p]

flatHead :: Con -> Pred
flatHead (CAll _ _ _ c) = flatHead c
flatHead (CHead p) = p
flatHead (CAnd _ _) = error "expected flat constraint"

mapFlatBody :: (Pred -> Pred) -> Con -> Con
mapFlatBody f (CAll x b p c) = CAll x b (f p) (mapFlatBody f c)
mapFlatBody _ (CHead p) = CHead p
mapFlatBody _ (CAnd _ _) = error "expected flat constraint"

mapFlatHead :: (Pred -> Pred) -> Con -> Con
mapFlatHead f (CAll x b p c) = CAll x b p (mapFlatHead f c)
mapFlatHead f (CHead p) = CHead (f p)
mapFlatHead _ (CAnd _ _) = error "expected flat constraint"

-- | A Horn assignment σ mapping Horn variables κ to predicates over the Horn
-- variables parameters x̄.
type Assignment = Map Name ([Name], Pred)

-- | Apply a Horn assignment σ to a predicate, replacing each Horn application
-- κ(ȳ) with its solution σ(κ)[x̄/ȳ].
apply :: Assignment -> Pred -> Pred
apply s = \case
  PVal x       -> PVal x
  PBin o p1 p2 -> PBin o (apply s p1) (apply s p2)
  PRel r p1 p2 -> PRel r (apply s p1) (apply s p2)
  PAnd ps      -> PAnd (map (apply s) ps)
  PDisj p1 p2  -> PDisj (apply s p1) (apply s p2)
  PImpl p1 p2  -> PImpl (apply s p1) (apply s p2)
  PIff p1 p2   -> PIff (apply s p1) (apply s p2)
  PNot p       -> PNot (apply s p)
  PFun f ps    -> PFun f (map (apply s) ps)
  PHorn k xs   -> case Map.lookup k s of
    Just (ys,p) -> substN xs ys p
    Nothing     -> PHorn k xs
  PExists x b p -> PExists x b (apply s p)

applyCon :: Assignment -> Con -> Con
applyCon s = \case
  CAll x b p c -> CAll x b (apply s p) (applyCon s c)
  CAnd c1 c2 -> CAnd (applyCon s c1) (applyCon s c2)
  CHead p -> CHead (apply s p)

substN :: [Value] -> [Name] -> Pred -> Pred
substN (x:xs) (y:ys) = substN xs ys . subst x y
substN _ _ = id

------------------------------------------------------------------------------

smtValid :: [Con] -> IO Bool
smtValid cs = do
  let foralls = map (Text.unpack . printSMTLib2) cs
  let declares = 
        [ "(declare-fun length (String) Int)"
        , "(declare-fun substring (String Int Int) String)"
        ]
  let asserts = map (\f -> "(assert " ++ f ++ ")") foralls
  let query = unlines $ declares ++ asserts ++ ["(check-sat)"]
  putStrLn query
  (code, output, _) <- readProcessWithExitCode "z3" ["-smt2", "-in"] query
  case code of
    ExitSuccess -> case dropWhileEnd isSpace output of
      "sat" -> return True
      "unsat" -> return False
      x -> error x    
    ExitFailure _ -> error output

------------------------------------------------------------------------------

printSMTLib2 :: SMTLib2 a => a -> Text
printSMTLib2 = LT.toStrict . LB.toLazyText . encode

class SMTLib2 a where
  encode :: a -> Builder

parens :: Builder -> Builder
parens a = "(" <> a <> ")"

(<+>) :: Builder -> Builder -> Builder
(<+>) a b = a <> " " <> b

sexpr :: [Builder] -> Builder
sexpr  = parens . foldr1 (<+>)

instance SMTLib2 Con where
  encode (CHead p) = encode p
  encode (CAnd c1 c2) = sexpr ["and", encode c1, encode c2]
  encode (CAll x b p c) = sexpr ["forall", sexpr [sort], impl]
    where
      sort = sexpr [encode x, encode b]
      impl = sexpr ["=>", encode p, encode c]

instance SMTLib2 Pred where
  encode (PVal v) = encode v
  encode (PBin o p1 p2) = sexpr [encode o, encode p1, encode p2]
  encode (PRel r p1 p2) = sexpr [encode r, encode p1, encode p2]
  encode (PAnd ps) = sexpr ("and" : map encode ps)
  encode (PDisj p1 p2) = sexpr ["or", encode p1, encode p2]
  encode (PImpl p1 p2) = sexpr ["=>", encode p1, encode p2]
  encode (PIff p1 p2) = sexpr ["iff", encode p1, encode p2]
  encode (PNot p) = sexpr ["not", encode p]
  encode (PFun x ps) = sexpr (encode x : map encode ps)
  encode (PHorn x vs) = sexpr (encode x : map encode vs)
  encode (PExists x b p) = sexpr ["exists", sexpr [sort], encode p]
    where
      sort = sexpr [encode x, encode b]

instance SMTLib2 Name where
  encode (Name n _) = LB.fromText n

instance SMTLib2 Value where
  encode (U _) = "0"  -- TODO
  encode (B True _) = "true"
  encode (B False _) = "false"
  encode (I x _) = LB.fromString (show x)
  encode (S x _) = LB.fromString (show x)
  encode (V x) = encode x

instance SMTLib2 Bop where
  encode Add = "+"
  encode Sub = "-"
  encode Mul = "*"
  encode Div = "/"

instance SMTLib2 Rel where
  encode Eq = "="
  encode Neq = "disequal"
  encode Geq = ">="
  encode Leq = "<="
  encode Gt = ">"
  encode Lt = "<"

instance SMTLib2 Base where
  encode TUnit = "Int"  -- TODO
  encode TBool = "Bool"
  encode TInt = "Int"
  encode TString = "String"

