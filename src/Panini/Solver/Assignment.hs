-- TODO: module documentation
module Panini.Solver.Assignment where

import Data.Map (Map)
import Data.Map qualified as Map
import Panini.Syntax
import Prelude


-- TODO: the mapping should be just to Map K Pred, because the params are always
-- implicitly named (z1,...,zn)

-- | A Horn assignment σ mapping Horn variables κ to predicates over the Horn
-- variables parameters x̄.
type Assignment = Map KVar ([Name], Pred)

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
  PAppK k xs   -> case Map.lookup k s of
    Just (ys,p) -> substN xs ys p
    Nothing     -> PAppK k xs
  PExists x b p -> PExists x b (apply s p)

applyCon :: Assignment -> Con -> Con
applyCon s = \case
  CAll x b p c -> CAll x b (apply s p) (applyCon s c)
  CAnd c1 c2 -> CAnd (applyCon s c1) (applyCon s c2)
  CHead p -> CHead (apply s p)

substN :: [Name] -> [Name] -> Pred -> Pred
substN (x:xs) (y:ys) = substN xs ys . subst x y
substN _ _ = id


-- prettyAss :: Assignment -> String
-- prettyAss = unlines . map go . Map.toAscList
--   where
--     go (k,(xs,p)) = showPretty k ++ " = " ++ concatMap lam xs ++ showPretty (simplify p)
--     lam x = "λ" ++ showPretty x ++ "."