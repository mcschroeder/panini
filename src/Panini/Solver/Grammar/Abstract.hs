module Panini.Solver.Grammar.Abstract where

import Panini.Syntax
import Prelude
import Panini.Solver.Abstract.ABool
import Panini.Solver.Abstract.AInt
import Panini.Solver.Abstract.AChar
import Panini.Solver.Abstract.AString
import Data.Text qualified as Text
import Panini.Algebra.Lattice
import Panini.Pretty.Printer
-- import Debug.Trace

-------------------------------------------------------------------------------

-- TODO
eval :: PExpr -> PExpr
eval (PAdd (PCon (I 0 _)) e) = e
eval (PAdd (PAbs (AInt a)) (PCon (I i _))) = PAbs $ AInt $ aIntegerAddI a i
eval (PAdd (PCon (I i _)) (PAbs (AInt a))) = PAbs $ AInt $ aIntegerAddI a i
eval e = e

-- | Abstract Semantics of Constrained Variables ⟦□⟧↑□
abstractVar :: Name -> Base -> Pred -> PExpr
abstractVar x b p
  | x `notElem` (freeVars p) = topExpr b
  | PRel r e1 e2 <- p, x `notElem` (freeVars e1) = abstractVar x b $ PRel (convRel r) e2 e1  
  | otherwise = case p of    
    -- TReg (PVar _) re -> PAbs $ AString $ undefined re -- TODO           -- x ∈ RE
    
    -- x + a ⋈ b
    PRel r (PAdd lhs1 lhs2) rhs1 
      | x `elem` (freeVars lhs1) -> abstractVar x b $ PRel r lhs1 $ eval (PSub rhs1 lhs2)

    -- x - a ⋈ b
    PRel r (PSub lhs1 lhs2) rhs1 
      | x `elem` (freeVars lhs1) -> abstractVar x b $ PRel r lhs1 $ eval (PAdd rhs1 lhs2)



    PRel Eq (PVar _) (PCon (B a _)) -> PAbs $ ABool $ aBoolEq a         -- x = a
    PRel Ne (PVar _) (PCon (B a _)) -> PAbs $ ABool $ aBoolEq (not a)   -- x ≠ a
    
    PRel Eq (PVar _) (PCon (I a _)) -> PAbs $ AInt $ aIntegerEq a       -- x = a
    PRel Ne (PVar _) (PCon (I a _)) -> PAbs $ AInt $ aIntegerNe a       -- x ≠ a
    PRel Gt (PVar _) (PCon (I a _)) -> PAbs $ AInt $ aIntegerGt a       -- x > a
    PRel Ge (PVar _) (PCon (I a _)) -> PAbs $ AInt $ aIntegerGe a       -- x ≥ a
    PRel Lt (PVar _) (PCon (I a _)) -> PAbs $ AInt $ aIntegerLt a       -- x < a
    PRel Le (PVar _) (PCon (I a _)) -> PAbs $ AInt $ aIntegerLe a       -- x ≤ a

    PRel Eq (PVar _) (PConChar c) -> PAbs $ AString $ aStringLit (aCharEq c) -- x = "c"
    PRel Ne (PVar _) (PConChar c) -> PAbs $ AString $ aStringLit (aCharNe c) -- x ≠ "c"

    -- x[i] = c
    PRel Eq (PStrAt (PVar _) (PCon (I i _))) (PConChar c) ->
      PAbs $ AString $ mconcat [ aStringRep aStringSigma i
                               , aStringLit (aCharEq c)
                               , aStringStar aStringSigma]  -- Σ^(i-1)cΣ*

    -- x[i] ≠ c
    PRel Ne (PStrAt (PVar _) (PCon (I i _))) (PConChar c) -> 
      PAbs $ AString $ mconcat $ [ aStringRep aStringSigma i
                                 , aStringLit (aCharNe c)
                                 , aStringStar aStringSigma]

    -- TODO: generalize string length abstractions via abstract int

    -- TODO: find general solution
    -- |s| = [a,∞]
    PRel Eq (PStrLen _) (PAbs (AInt i))
      | Just (Fin a) <- aMinimum i
      , Just PosInf <- aMaximum i
      , aContinuous i
      -> PAbs $ AString $ mconcat [ aStringRep aStringSigma a
                                  , aStringStar aStringSigma ]



    -- |s| = i
    PRel Eq (PStrLen (PVar _)) (PCon (I i _)) ->
      PAbs $ AString $ aStringRep aStringSigma i  -- Σ^i

    -- |s| ≠ i
    PRel Ne (PStrLen (PVar _)) (PCon (I i _)) ->
      PAbs $ AString $ joins1  -- Σ^(i-1) | Σ^(i+1)Σ*
        [ aStringRep aStringSigma (i-1)
        , mconcat [ aStringRep aStringSigma i
                  , aStringStar aStringSigma]  
        ]

    -- |s| ≥ i
    PRel Ge (PStrLen (PVar _)) (PCon (I i _)) ->
      PAbs $ AString $ mconcat [ aStringRep aStringSigma i
                               , aStringStar aStringSigma]  -- Σ^iΣ*

    PRel Ge (PStrLen (PVar _)) (PAbs (AInt a)) ->
      case aMinimum (a ∧ aIntegerGe 0) of
        Just (Fin i) -> PAbs $ AString $ mconcat [ aStringRep aStringSigma i
                               , aStringStar aStringSigma]  -- Σ^iΣ*
        _ -> PAbs $ AString (⊥)

    -- |s| > i
    PRel Gt (PStrLen (PVar _)) (PCon (I i _)) ->
      PAbs $ AString $ mconcat [ aStringRep aStringSigma (i + 1)
                               , aStringStar aStringSigma]  -- Σ^iΣ*                           

    -- TODO: hardcoded hack?
    -- |s| < 0
    PRel Lt (PStrLen (PVar _)) (PCon (I 0 _)) -> PAbs $ AString (⊥)
    PRel Lt (PStrLen (PVar _)) (PAbs (AInt a)) 
      | aMinimum a < Just (Fin 0) -> PAbs $ AString (⊥)
    
    -- TODO: ???? I don't know about these...
    PRel Eq (PVar _) (PVar y) -> PVar y          -- x = y
    PRel Ne (PVar _) (PVar y) -> PNot2 (PVar y)   -- x ≠ y

    -- x = |s|
    PRel Eq (PVar _) e@(PStrLen (PVar _)) -> e
    
    -- x ≠ |s|
    PRel Ne (PVar _) e@(PStrLen (PVar _)) ->
      PAdd e (PAbs $ AInt $ aIntegerNe 0)  -- |s| + [-∞,-1|1,∞]

    -- x < |s|
    PRel Lt (PVar _) e@(PStrLen (PVar _)) -> 
      PSub e (PAbs $ AInt $ aIntegerGe 1) -- |s| - [1,∞]
    
    -- x > |s|
    PRel Gt (PVar _) e@(PStrLen (PVar _)) -> 
      PAdd e (PAbs $ AInt $ aIntegerGe 1) -- |s| + [1,∞]

    -- x ≥ |s|
    PRel Ge (PVar _) e@(PStrLen (PVar _)) -> 
      PAdd e (PAbs $ AInt $ aIntegerGe 0) -- |s| + [0,∞]


    -- TODO: ???? I don't know about these...
    PRel Eq (PVar _) e@(PStrAt (PVar _) (PCon _)) -> e       -- x = s[i]
    PRel Ne (PVar _) e@(PStrAt (PVar _) (PCon _)) -> PNot2 e  -- x ≠ s[i]

    _ -> error $ "abstraction impossible: ⟦" ++ showPretty p ++ "⟧↑" ++ showPretty x

topExpr :: Base -> PExpr
topExpr TBool = PAbs $ ABool (⊤)
topExpr TInt = PAbs $ AInt (⊤)
topExpr TString = PAbs $ AString (⊤)
topExpr _ = undefined

concretizeVar :: Name -> PExpr -> Pred
concretizeVar x e = case e of
  PAbs (AString s) -> PReg (Var x) (showPretty s)
  _ -> error $ "concretization impossible (or not yet implemented): ⟦" ++ showPretty e ++ "⟧↓" ++ showPretty x

-- -- | Constraint Re-Concretization ⟦□⟧↓□
-- concretizeVar :: Name -> PExpr -> Tree
-- concretizeVar x e = case e of
--   PAbs (AString s) -> TPred $ PReg (PVar x) $ RE $ showPretty s  -- TODO
--   PAbs _           -> error $ "concretization impossible: ⟦" ++ showPretty e ++ "⟧↓" ++ showPretty x
--   _                -> TPred $ PRel Eq (PVar x) e  -- TODO: ensure e not abstract

pattern PConChar :: Char -> PExpr
pattern PConChar c <- PCon (S (Text.unpack -> [c]) _)