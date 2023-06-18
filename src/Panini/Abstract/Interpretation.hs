module Panini.Abstract.Interpretation where

import Data.Text qualified as Text
import Panini.Abstract.ABool
import Panini.Abstract.AChar
import Panini.Abstract.AInt
import Panini.Abstract.AString
import Panini.Abstract.AValue
import Panini.Algebra.Lattice
import Panini.Logic.Expressions
import Panini.Logic.Relations
import Panini.Names
import Panini.Pretty.Printer
import Panini.Primitives
import Panini.Substitution
import Prelude
-- import Debug.Trace

-------------------------------------------------------------------------------

-- TODO
eval :: PExpr -> PExpr
eval (PAdd (PCon (I 0 _)) e) = e
eval (PAdd (PAbs (AInt a)) (PCon (I i _))) = PAbs $ AInt $ aIntegerAddI a i
eval (PAdd (PCon (I i _)) (PAbs (AInt a))) = PAbs $ AInt $ aIntegerAddI a i
eval e = e

-- | Abstract Semantics of Constrained Variables ⟦□⟧↑□
abstractVar :: Name -> Base -> Rel -> PExpr
abstractVar x b p -- TODO: rename p to r
  | x `notElem` freeVars p = topExpr b
  | x `notElem` freeVars (leftSide p), Just r' <- converse p = abstractVar x b r'
  | otherwise = case p of    
    -- TReg (PVar _) re -> PAbs $ AString $ undefined re -- TODO           -- x ∈ RE
    
    -- x + a ⋈ b
    Rel r (PAdd lhs1 lhs2) rhs1 
      | x `elem` (freeVars lhs1) -> abstractVar x b $ Rel r lhs1 $ eval (PSub rhs1 lhs2)

    -- x - a ⋈ b
    Rel r (PSub lhs1 lhs2) rhs1 
      | x `elem` (freeVars lhs1) -> abstractVar x b $ Rel r lhs1 $ eval (PAdd rhs1 lhs2)



    Rel Eq (PVar _) (PCon (B a _)) -> PAbs $ ABool $ aBoolEq a         -- x = a
    Rel Ne (PVar _) (PCon (B a _)) -> PAbs $ ABool $ aBoolEq (not a)   -- x ≠ a
    
    Rel Eq (PVar _) (PCon (I a _)) -> PAbs $ AInt $ aIntegerEq a       -- x = a
    Rel Ne (PVar _) (PCon (I a _)) -> PAbs $ AInt $ aIntegerNe a       -- x ≠ a
    Rel Gt (PVar _) (PCon (I a _)) -> PAbs $ AInt $ aIntegerGt a       -- x > a
    Rel Ge (PVar _) (PCon (I a _)) -> PAbs $ AInt $ aIntegerGe a       -- x ≥ a
    Rel Lt (PVar _) (PCon (I a _)) -> PAbs $ AInt $ aIntegerLt a       -- x < a
    Rel Le (PVar _) (PCon (I a _)) -> PAbs $ AInt $ aIntegerLe a       -- x ≤ a

    Rel Eq (PVar _) (PConChar c) -> PAbs $ AString $ aStringLit (aCharEq c) -- x = "c"
    Rel Ne (PVar _) (PConChar c) -> PAbs $ AString $ aStringLit (aCharNe c) -- x ≠ "c"

    -- x[i] = c
    Rel Eq (PStrAt (PVar _) (PCon (I i _))) (PConChar c) ->
      PAbs $ AString $ mconcat [ aStringRep aStringSigma i
                               , aStringLit (aCharEq c)
                               , aStringStar aStringSigma]  -- Σ^(i-1)cΣ*

    -- x[i] ≠ c
    Rel Ne (PStrAt (PVar _) (PCon (I i _))) (PConChar c) -> 
      PAbs $ AString $ mconcat $ [ aStringRep aStringSigma i
                                 , aStringLit (aCharNe c)
                                 , aStringStar aStringSigma]

    -- TODO: generalize string length abstractions via abstract int

    -- TODO: find general solution
    -- |s| = [a,∞]
    Rel Eq (PStrLen _) (PAbs (AInt i))
      | Just (Fin a) <- aMinimum i
      , Just PosInf <- aMaximum i
      , aContinuous i
      -> PAbs $ AString $ mconcat [ aStringRep aStringSigma a
                                  , aStringStar aStringSigma ]



    -- |s| = i
    Rel Eq (PStrLen (PVar _)) (PCon (I i _)) ->
      PAbs $ AString $ aStringRep aStringSigma i  -- Σ^i

    -- |s| ≠ i
    Rel Ne (PStrLen (PVar _)) (PCon (I i _)) ->
      PAbs $ AString $ joins1  -- Σ^(i-1) | Σ^(i+1)Σ*
        [ aStringRep aStringSigma (i-1)
        , mconcat [ aStringRep aStringSigma i
                  , aStringStar aStringSigma]  
        ]

    -- |s| ≥ i
    Rel Ge (PStrLen (PVar _)) (PCon (I i _)) ->
      PAbs $ AString $ mconcat [ aStringRep aStringSigma i
                               , aStringStar aStringSigma]  -- Σ^iΣ*

    Rel Ge (PStrLen (PVar _)) (PAbs (AInt a)) ->
      case aMinimum (a ∧ aIntegerGe 0) of
        Just (Fin i) -> PAbs $ AString $ mconcat [ aStringRep aStringSigma i
                               , aStringStar aStringSigma]  -- Σ^iΣ*
        _ -> PAbs $ AString bot

    -- |s| > i
    Rel Gt (PStrLen (PVar _)) (PCon (I i _)) ->
      PAbs $ AString $ mconcat [ aStringRep aStringSigma (i + 1)
                               , aStringStar aStringSigma]  -- Σ^iΣ*                           

    -- TODO: hardcoded hack?
    -- |s| < 0
    Rel Lt (PStrLen (PVar _)) (PCon (I 0 _)) -> PAbs $ AString bot
    Rel Lt (PStrLen (PVar _)) (PAbs (AInt a)) 
      | aMinimum a < Just (Fin 0) -> PAbs $ AString bot
    
    -- TODO: ???? I don't know about these...
    Rel Eq (PVar _) (PVar y) -> PVar y          -- x = y
    Rel Ne (PVar _) (PVar y) -> PNot2 (PVar y)   -- x ≠ y

    -- x = |s|
    Rel Eq (PVar _) e@(PStrLen (PVar _)) -> e
    
    -- x ≠ |s|
    Rel Ne (PVar _) e@(PStrLen (PVar _)) ->
      PAdd e (PAbs $ AInt $ aIntegerNe 0)  -- |s| + [-∞,-1|1,∞]

    -- x < |s|
    Rel Lt (PVar _) e@(PStrLen (PVar _)) -> 
      PSub e (PAbs $ AInt $ aIntegerGe 1) -- |s| - [1,∞]
    
    -- x > |s|
    Rel Gt (PVar _) e@(PStrLen (PVar _)) -> 
      PAdd e (PAbs $ AInt $ aIntegerGe 1) -- |s| + [1,∞]

    -- x ≥ |s|
    Rel Ge (PVar _) e@(PStrLen (PVar _)) -> 
      PAdd e (PAbs $ AInt $ aIntegerGe 0) -- |s| + [0,∞]


    -- TODO: ???? I don't know about these...
    Rel Eq (PVar _) e@(PStrAt (PVar _) (PCon _)) -> e       -- x = s[i]
    Rel Ne (PVar _) e@(PStrAt (PVar _) (PCon _)) -> PNot2 e  -- x ≠ s[i]

    _ -> error $ "abstraction impossible: ⟦" ++ showPretty p ++ "⟧↑" ++ showPretty x

topExpr :: Base -> PExpr
topExpr TBool = PAbs $ ABool top
topExpr TInt = PAbs $ AInt top
topExpr TString = PAbs $ AString top
topExpr _ = undefined

concretizeVar :: Name -> PExpr -> Rel
concretizeVar x e = case e of
  PAbs (AString s) -> Rel In (PVar x) (PAbs (AString s))
  _ -> error $ "concretization impossible (or not yet implemented): ⟦" ++ showPretty e ++ "⟧↓" ++ showPretty x

-- -- | Constraint Re-Concretization ⟦□⟧↓□
-- concretizeVar :: Name -> PExpr -> Tree
-- concretizeVar x e = case e of
--   PAbs (AString s) -> TPred $ PReg (PVar x) $ RE $ showPretty s  -- TODO
--   PAbs _           -> error $ "concretization impossible: ⟦" ++ showPretty e ++ "⟧↓" ++ showPretty x
--   _                -> TPred $ Rel Eq (PVar x) e  -- TODO: ensure e not abstract

pattern PConChar :: Char -> PExpr
pattern PConChar c <- PCon (S (Text.unpack -> [c]) _)