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
eval :: Expr -> Expr
eval (EAdd (ECon (I 0 _)) e) = e
eval (EAdd (EAbs (AInt a)) (ECon (I i _))) = EAbs $ AInt $ aIntegerAddI a i
eval (EAdd (ECon (I i _)) (EAbs (AInt a))) = EAbs $ AInt $ aIntegerAddI a i
eval e = e

-- | Abstract Semantics of Constrained Variables ⟦□⟧↑□
abstractVar :: Name -> Base -> Rel -> Expr
abstractVar x b p -- TODO: rename p to r
  | x `notElem` freeVars p = topExpr b
  | x `notElem` freeVars (leftSide p), Just r' <- converse p = abstractVar x b r'
  | otherwise = case p of    
    -- TReg (EVar _) re -> EAbs $ AString $ undefined re -- TODO           -- x ∈ RE
    
    -- x + a ⋈ b
    Rel r (EAdd lhs1 lhs2) rhs1 
      | x `elem` (freeVars lhs1) -> abstractVar x b $ Rel r lhs1 $ eval (ESub rhs1 lhs2)

    -- x - a ⋈ b
    Rel r (ESub lhs1 lhs2) rhs1 
      | x `elem` (freeVars lhs1) -> abstractVar x b $ Rel r lhs1 $ eval (EAdd rhs1 lhs2)



    Rel Eq (EVar _) (ECon (B a _)) -> EAbs $ ABool $ aBoolEq a         -- x = a
    Rel Ne (EVar _) (ECon (B a _)) -> EAbs $ ABool $ aBoolEq (not a)   -- x ≠ a
    
    Rel Eq (EVar _) (ECon (I a _)) -> EAbs $ AInt $ aIntegerEq a       -- x = a
    Rel Ne (EVar _) (ECon (I a _)) -> EAbs $ AInt $ aIntegerNe a       -- x ≠ a
    Rel Gt (EVar _) (ECon (I a _)) -> EAbs $ AInt $ aIntegerGt a       -- x > a
    Rel Ge (EVar _) (ECon (I a _)) -> EAbs $ AInt $ aIntegerGe a       -- x ≥ a
    Rel Lt (EVar _) (ECon (I a _)) -> EAbs $ AInt $ aIntegerLt a       -- x < a
    Rel Le (EVar _) (ECon (I a _)) -> EAbs $ AInt $ aIntegerLe a       -- x ≤ a

    Rel Eq (EVar _) (PConChar c) -> EAbs $ AString $ aStringLit (aCharEq c) -- x = "c"
    Rel Ne (EVar _) (PConChar c) -> EAbs $ AString $ aStringLit (aCharNe c) -- x ≠ "c"

    -- x[i] = c
    Rel Eq (EStrAt (EVar _) (ECon (I i _))) (PConChar c) ->
      EAbs $ AString $ mconcat [ aStringRep aStringSigma i
                               , aStringLit (aCharEq c)
                               , aStringStar aStringSigma]  -- Σ^(i-1)cΣ*

    -- x[i] ≠ c
    Rel Ne (EStrAt (EVar _) (ECon (I i _))) (PConChar c) -> 
      EAbs $ AString $ mconcat $ [ aStringRep aStringSigma i
                                 , aStringLit (aCharNe c)
                                 , aStringStar aStringSigma]

    -- TODO: generalize string length abstractions via abstract int

    -- TODO: find general solution
    -- |s| = [a,∞]
    Rel Eq (EStrLen _) (EAbs (AInt i))
      | Just (Fin a) <- aMinimum i
      , Just PosInf <- aMaximum i
      , aContinuous i
      -> EAbs $ AString $ mconcat [ aStringRep aStringSigma a
                                  , aStringStar aStringSigma ]



    -- |s| = i
    Rel Eq (EStrLen (EVar _)) (ECon (I i _)) ->
      EAbs $ AString $ aStringRep aStringSigma i  -- Σ^i

    -- |s| ≠ i
    Rel Ne (EStrLen (EVar _)) (ECon (I i _)) ->
      EAbs $ AString $ joins1  -- Σ^(i-1) | Σ^(i+1)Σ*
        [ aStringRep aStringSigma (i-1)
        , mconcat [ aStringRep aStringSigma i
                  , aStringStar aStringSigma]  
        ]

    -- |s| ≥ i
    Rel Ge (EStrLen (EVar _)) (ECon (I i _)) ->
      EAbs $ AString $ mconcat [ aStringRep aStringSigma i
                               , aStringStar aStringSigma]  -- Σ^iΣ*

    Rel Ge (EStrLen (EVar _)) (EAbs (AInt a)) ->
      case aMinimum (a ∧ aIntegerGe 0) of
        Just (Fin i) -> EAbs $ AString $ mconcat [ aStringRep aStringSigma i
                               , aStringStar aStringSigma]  -- Σ^iΣ*
        _ -> EAbs $ AString bot

    -- |s| > i
    Rel Gt (EStrLen (EVar _)) (ECon (I i _)) ->
      EAbs $ AString $ mconcat [ aStringRep aStringSigma (i + 1)
                               , aStringStar aStringSigma]  -- Σ^iΣ*                           

    -- TODO: hardcoded hack?
    -- |s| < 0
    Rel Lt (EStrLen (EVar _)) (ECon (I 0 _)) -> EAbs $ AString bot
    Rel Lt (EStrLen (EVar _)) (EAbs (AInt a)) 
      | aMinimum a < Just (Fin 0) -> EAbs $ AString bot
    
    -- TODO: ???? I don't know about these...
    Rel Eq (EVar _) (EVar y) -> EVar y          -- x = y
    Rel Ne (EVar _) (EVar y) -> ENot (EVar y)   -- x ≠ y

    -- x = |s|
    Rel Eq (EVar _) e@(EStrLen (EVar _)) -> e
    
    -- x ≠ |s|
    Rel Ne (EVar _) e@(EStrLen (EVar _)) ->
      EAdd e (EAbs $ AInt $ aIntegerNe 0)  -- |s| + [-∞,-1|1,∞]

    -- x < |s|
    Rel Lt (EVar _) e@(EStrLen (EVar _)) -> 
      ESub e (EAbs $ AInt $ aIntegerGe 1) -- |s| - [1,∞]
    
    -- x > |s|
    Rel Gt (EVar _) e@(EStrLen (EVar _)) -> 
      EAdd e (EAbs $ AInt $ aIntegerGe 1) -- |s| + [1,∞]

    -- x ≥ |s|
    Rel Ge (EVar _) e@(EStrLen (EVar _)) -> 
      EAdd e (EAbs $ AInt $ aIntegerGe 0) -- |s| + [0,∞]


    -- TODO: ???? I don't know about these...
    Rel Eq (EVar _) e@(EStrAt (EVar _) (ECon _)) -> e       -- x = s[i]
    Rel Ne (EVar _) e@(EStrAt (EVar _) (ECon _)) -> ENot e  -- x ≠ s[i]

    _ -> error $ "abstraction impossible: ⟦" ++ showPretty p ++ "⟧↑" ++ showPretty x

topExpr :: Base -> Expr
topExpr TBool = EAbs $ ABool top
topExpr TInt = EAbs $ AInt top
topExpr TString = EAbs $ AString top
topExpr _ = undefined

concretizeVar :: Name -> Expr -> Rel
concretizeVar x e = case e of
  EAbs (AString s) -> Rel In (EVar x) (EAbs (AString s))
  _ -> error $ "concretization impossible (or not yet implemented): ⟦" ++ showPretty e ++ "⟧↓" ++ showPretty x

-- -- | Constraint Re-Concretization ⟦□⟧↓□
-- concretizeVar :: Name -> Expr -> Tree
-- concretizeVar x e = case e of
--   EAbs (AString s) -> TPred $ PReg (EVar x) $ RE $ showPretty s  -- TODO
--   EAbs _           -> error $ "concretization impossible: ⟦" ++ showPretty e ++ "⟧↓" ++ showPretty x
--   _                -> TPred $ Rel Eq (EVar x) e  -- TODO: ensure e not abstract

pattern PConChar :: Char -> Expr
pattern PConChar c <- ECon (S (Text.unpack -> [c]) _)