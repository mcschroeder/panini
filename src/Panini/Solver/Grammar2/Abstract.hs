module Panini.Solver.Grammar2.Abstract where

import Panini.Solver.Grammar2.Tree
import Panini.Syntax
import Prelude
import Panini.Solver.Abstract.ABool
import Panini.Solver.Abstract.AInt
import Panini.Solver.Abstract.AChar
import Panini.Solver.Abstract.AString
import Data.Text qualified as Text
import Panini.Algebra.Lattice
import Panini.Pretty.Printer
import Debug.Trace

-------------------------------------------------------------------------------

-- TODO
eval :: TExpr -> TExpr
eval (TAdd (TCon (I 0 _)) e) = e
eval e = e

-- | Abstract Semantics of Constrained Variables ⟦□⟧↑□
abstractVar :: Name -> Base -> TPred -> TExpr
abstractVar x b p
  | x `notElem` (varsP p) = topExpr b
  | TRel r e1 e2 <- p, x `notElem` (varsE e1) = abstractVar x b $ TRel (convRel r) e2 e1  
  | otherwise = case p of    
    TReg (TVar _) re -> TAbs $ AString $ undefined re -- TODO           -- x ∈ RE
    
    -- x + a ⋈ b
    TRel r (TAdd lhs1 lhs2) rhs1 
      | x `elem` (varsE lhs1) -> abstractVar x b $ TRel r lhs1 $ eval (TAdd rhs1 lhs2)


    TRel Eq (TVar _) (TCon (B a _)) -> TAbs $ ABool $ aBoolEq a         -- x = a
    TRel Ne (TVar _) (TCon (B a _)) -> TAbs $ ABool $ aBoolEq (not a)   -- x ≠ a
    
    TRel Eq (TVar _) (TCon (I a _)) -> TAbs $ AInt $ aIntegerEq a       -- x = a
    TRel Ne (TVar _) (TCon (I a _)) -> TAbs $ AInt $ aIntegerNe a       -- x ≠ a
    TRel Gt (TVar _) (TCon (I a _)) -> TAbs $ AInt $ aIntegerGt a       -- x > a
    TRel Ge (TVar _) (TCon (I a _)) -> TAbs $ AInt $ aIntegerGe a       -- x ≥ a
    TRel Lt (TVar _) (TCon (I a _)) -> TAbs $ AInt $ aIntegerLt a       -- x < a
    TRel Le (TVar _) (TCon (I a _)) -> TAbs $ AInt $ aIntegerLe a       -- x ≤ a

    TRel Eq (TVar _) (TConChar c) -> TAbs $ AString $ aStringLit (aCharEq c) -- x = "c"
    TRel Ne (TVar _) (TConChar c) -> TAbs $ AString $ aStringLit (aCharNe c) -- x ≠ "c"

    -- x[i] = c
    TRel Eq (TStrAt (TVar _) (TCon (I i _))) (TConChar c) ->
      TAbs $ AString $ mconcat [ aStringRep aStringSigma (i-1)
                               , aStringLit (aCharEq c)
                               , aStringStar aStringSigma]  -- Σ^(i-1)cΣ*

    -- x[i] ≠ c
    TRel Ne (TStrAt (TVar _) (TCon (I i _))) (TConChar c) -> 
      TAbs $ AString $ mconcat $ [ aStringRep aStringSigma (i-1)
                                 , aStringLit (aCharNe c)
                                 , aStringStar aStringSigma]

    -- TODO: generalize string length abstractions via abstract int

    -- |s| = i
    TRel Eq (TStrLen (TVar _)) (TCon (I i _)) ->
      TAbs $ AString $ aStringRep aStringSigma i  -- Σ^i

    -- |s| ≠ i
    TRel Ne (TStrLen (TVar _)) (TCon (I i _)) ->
      TAbs $ AString $ joins1  -- Σ^(i-1) | Σ^(i+1)Σ*
        [ aStringRep aStringSigma (i-1)
        , mconcat [ aStringRep aStringSigma i
                  , aStringStar aStringSigma]  
        ]

    -- |s| ≥ i
    TRel Ge (TStrLen (TVar _)) (TCon (I i _)) ->
      TAbs $ AString $ mconcat [ aStringRep aStringSigma i
                               , aStringStar aStringSigma]  -- Σ^iΣ*

    TRel Ge (TStrLen (TVar _)) (TAbs (AInt a)) ->
      case aMinimum (a ∧ aIntegerGe 0) of
        Just (Fin i) -> TAbs $ AString $ mconcat [ aStringRep aStringSigma i
                               , aStringStar aStringSigma]  -- Σ^iΣ*
        _ -> TAbs $ AString (⊥)

    -- |s| > i
    TRel Gt (TStrLen (TVar _)) (TCon (I i _)) ->
      TAbs $ AString $ mconcat [ aStringRep aStringSigma (i + 1)
                               , aStringStar aStringSigma]  -- Σ^iΣ*                           

    -- TODO: hardcoded hack?
    -- |s| < 0
    TRel Lt (TStrLen (TVar _)) (TCon (I 0 _)) -> TAbs $ AString (⊥)
    TRel Lt (TStrLen (TVar _)) (TAbs (AInt a)) 
      | aMinimum a < Just (Fin 0) -> TAbs $ AString (⊥)
    
    -- TODO: ???? I don't know about these...
    TRel Eq (TVar _) (TVar y) -> TVar y          -- x = y
    TRel Ne (TVar _) (TVar y) -> TNot (TVar y)   -- x ≠ y

    -- x = |s|
    TRel Eq (TVar _) e@(TStrLen (TVar _)) -> e
    
    -- x ≠ |s|
    TRel Ne (TVar _) e@(TStrLen (TVar _)) ->
      TAdd e (TAbs $ AInt $ aIntegerNe 0)  -- |s| + [-∞,-1|1,∞]

    -- TODO: ???? I don't know about these...
    TRel Eq (TVar _) e@(TStrAt (TVar _) (TCon _)) -> e       -- x = s[i]
    TRel Ne (TVar _) e@(TStrAt (TVar _) (TCon _)) -> TNot e  -- x ≠ s[i]

    _ -> error $ "abstraction impossible: ⟦" ++ showPretty p ++ "⟧↑" ++ showPretty x

topExpr :: Base -> TExpr
topExpr TBool = TAbs $ ABool (⊤)
topExpr TInt = TAbs $ AInt (⊤)
topExpr TString = TAbs $ AString (⊤)
topExpr _ = undefined

-- | Constraint Re-Concretization ⟦□⟧↓□
concretizeVar :: Name -> TExpr -> Tree
concretizeVar x e = case e of
  TAbs (AString s) -> TPred $ TReg (TVar x) $ RE $ showPretty s  -- TODO
  TAbs _           -> error $ "concretization impossible: ⟦" ++ showPretty e ++ "⟧↓" ++ showPretty x
  _                -> TPred $ TRel Eq (TVar x) e  -- TODO: ensure e not abstract

pattern TConChar :: Char -> TExpr
pattern TConChar c <- TCon (S (Text.unpack -> [c]) _)