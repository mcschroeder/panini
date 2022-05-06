{-# LANGUAGE OverloadedStrings #-}

module Panini.SMT
  ( printSMTLib2
  , solve
  , flat
  , taut, simplify
  ) where

import Control.Monad
import Data.List (partition, dropWhileEnd)
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
import Panini.Substitution
import Panini.Syntax
import Prelude
import System.Process
import Data.Char
import System.Exit

------------------------------------------------------------------------------

-- | Solve a Horn constraint given a set of candidates.
solve :: Pred -> [Pred] -> IO (Maybe Assignment)
solve c _q = do
  let cs = filter (not . taut) $ map simplify $ flat c
  let (csk,csp) = partition horny cs
  --let s0 = Map.fromList $ map (\(k,xs) -> (k,(xs,pTrue))) $ getHornVars csk
  --let s0 = extractCandidateAssignment csk
  let qs = [ PRel Leq (pVar "z1") (PVal (I 0 NoPV))
           , PRel Geq (pVar "z1") (PVal (I 0 NoPV))
           , PRel Leq (pVar "z2") (PVal (I 0 NoPV))
           , PRel Geq (pVar "z2") (PVal (I 0 NoPV))
           , PRel Leq (pVar "z1") (pVar "z2")
           , PRel Geq (pVar "z2") (pVar "z1")
           ]
  let s0 = Map.fromList [("k0", (["z1","z2"], foldr pAnd (PTrue NoPV) qs))]
  putStrLn $ show s0
  s <- fixpoint csk s0
  r <- smtValid (map (apply s) csp)
  if r then return (Just s) else return Nothing

-- extractCandidateAssignment :: [Con] -> Assignment
-- extractCandidateAssignment = 
--   Map.fromListWith (\(xs,p1) (_,p2) -> (xs, p1 `pAnd` p2)) . map extractCandidate

-- extractCandidate :: Con -> (Name, ([Name],Pred))
-- extractCandidate = renameHornVarArgs . go []
--   where
--     go ps (CAll _ _ p c) = go (p:ps) c
--     go ps (CPred (PHorn k xs)) = (k, (v2n xs, foldr pAnd pTrue ps))
--     go _ _ = error "expected flat horn constraint"

--     v2n [] = []
--     v2n (V n:xs) = n:v2n xs
--     v2n _ = error "expected all Horn var arguments to be variables"

-- renameHornVarArgs :: (Name, ([Name], Pred)) -> (Name, ([Name], Pred))
-- renameHornVarArgs (k,(xs0,p0)) = let (p,zs) = go xs0 p0 [] in (k,(zs,p))
--   where
--     go [] p zs = (p, reverse zs)
--     go (x:xs) p zs = 
--       let z = freshName "z" (xs ++ freeVars p)
--           p' = subst (V z) x p
--       in go xs p' (z:zs)
    

-- getHornVars :: [Con] -> [(Name,[Name])]
-- getHornVars = map (f . getFlatHead)
--   where
--     f (PHorn k xs) = (k, g 0 xs)
--     f _ = error "expected flat constraint"

--     g !i (_:xs) = (fromString $ "z" ++ show @Int i) : g (i + 1) xs
--     g _ [] = []


-- | Flatten a Horn constraint into a set of flat constraints each of which is
-- of the form ∀xs:bs. p ⇒ p' where p' is either a single Horn application κ(ȳ)
-- or a concrete predicate free of Horn variables.
flat :: Pred -> [Pred]
flat p0 = [merge [] (PTrue NoPV) p' | p' <- split p0]
  where
    split (PAnd ps)     = concatMap split ps
    split (PImpl p q)   = [PImpl p q' | q' <- split q]
    split (PAll xs p)   = [PAll xs p' | p' <- split p]
    split p             = [p]

    merge xs p (PAll ys q) = 
      -- rename clashing bindings before merging the foralls
      let (ys1, q') = rename (map fst xs) (map fst ys) q
          ys2       = zip ys1 (map snd ys)
      in merge (xs ++ ys2) p q'
    
    merge xs p (PImpl q c) = merge xs (p `pAnd` q) c
    merge xs p q           = PAll xs (PImpl p q)

-- | @rename xs ys p@ renames all @ys@ in @p@ that clash with @xs@.
rename :: [Name] -> [Name] -> Pred -> ([Name], Pred)
rename xs = go []
  where
    go zs [] p = (zs, p)
    go zs (y:ys) p
      | y `elem` xs = let z = freshName y (xs ++ ys ++ freeVars p)
                          q = subst (V z) y p
                      in go (z:zs) ys q
      | otherwise = go (y:zs) ys p

-- TODO: preserve provenance information
simplify :: Pred -> Pred
simplify = \case
  PAnd (filter (not . taut) . map simplify -> ps)
    | []  <- ps -> PTrue NoPV
    | [p] <- ps -> p
    | otherwise -> PAnd ps

  PNot (simplify -> p) -> PNot p

  PImpl (simplify -> p) (simplify -> q)
    | PTrue _ <- p -> q
    | PTrue _ <- q -> PTrue NoPV
    | otherwise    -> PImpl p q
  
  PIff (simplify -> p) (simplify -> q)
    | PTrue _ <- p -> q
    | PTrue _ <- q -> p
    | otherwise    -> PIff p q
  
  PAll xs (simplify -> p) -> 
    -- remove redundant bindings that don't actually occur in the predicate
    let fvs = freeVars p
        xs' = filter (\(x,_) -> x `elem` fvs) xs
    in if null xs' then p else PAll xs' p
  
  p -> p

taut :: Pred -> Bool
taut (PTrue _)      = True
taut (PAnd [])      = True
taut (PAnd ps)      = all taut ps
taut (PRel Eq  p q) = p == q
taut (PRel Leq p q) = p == q
taut (PRel Geq p q) = p == q
taut _              = False


-- | Whether or not a flat constraint has a Horn application in its head.
horny :: Pred -> Bool
horny (PAll _ (PImpl _ (PHorn _ _))) = True
horny _ = False

-- | Iteratively weaken a candidate solution until an assignment satisfying all
-- given (flat) constraints is found.
fixpoint :: [Pred] -> Assignment -> IO Assignment
fixpoint cs s = do  
  r <- take 1 <$> filterM ((not <$>) . smtValid . pure . apply s) cs
  case r of
    [c] -> do
      s' <- weaken s c
      fixpoint cs s'
    _ -> return s

-- | Weaken a Horn assignment to satisfy a given (flat) constraint.
weaken :: Assignment -> Pred -> IO Assignment
weaken s c =
  case getFlatHead c of
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

getFlatHead :: Pred -> Pred
getFlatHead (PAll _ (PImpl _ q)) = getFlatHead q
getFlatHead p = p

mapFlatBody :: (Pred -> Pred) -> Pred -> Pred
mapFlatBody f (PAll xs (PImpl p q))
  | PAll _ _ <- q = PAll xs (PImpl (f p) (mapFlatBody f q))
  | otherwise     = PAll xs (PImpl (f p) q)
mapFlatBody _ _ = error "expected flat constraint"

mapFlatHead :: (Pred -> Pred) -> Pred -> Pred
mapFlatHead f (PAll xs (PImpl p q)) = PAll xs (PImpl p (mapFlatHead f q))
mapFlatHead f p = f p


-- | A Horn assignment σ mapping Horn variables κ to predicates over the Horn
-- variables parameters x̄.
type Assignment = Map Name ([Name], Pred)

-- | Apply a Horn assignment σ to a predicate, replacing each Horn application
-- κ(ȳ) with its solution σ(κ)[x̄/ȳ].
apply :: Assignment -> Pred -> Pred
apply s = \case
  PAll xs p    -> PAll xs (apply s p)
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

substN :: [Value] -> [Name] -> Pred -> Pred
substN (x:xs) (y:ys) = substN xs ys . subst x y
substN _ _ = id

------------------------------------------------------------------------------

smtValid :: [Pred] -> IO Bool
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
  encode (PAll xs p) = sexpr ["forall", sexpr (map sort xs), encode p] 
    where
      sort (x,b) = sexpr [encode x, encode b]

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

