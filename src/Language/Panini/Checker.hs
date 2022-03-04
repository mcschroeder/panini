module Language.Panini.Checker where

import Data.List

import Language.Panini.Syntax

-- | Type checker monad.
type TC a = Either TypeError a

failWith :: TypeError -> TC a
failWith = Left

data TypeError
  = InvalidSubtypingBase Base Base
  | InvalidSubtyping Type Type
  deriving (Show, Read)

------------------------------------------------------------------------------
-- Subtyping

{- | Subtyping rules.

@

  g |- forall v1:b. p1 => p2[v1/v2]
-------------------------------------  SUB-BASE
     g |- {v1:b|p1} <: {v2:b|p2}


  g |- s2 <: s1      g, x2:s2 |- t1[x2/x1] <: t2
--------------------------------------------------  SUB-FUN
          g |- x1:s1 -> t1 <: x2:s2 -> t2

@

-}
sub :: Type -> Type -> TC Con

-- [SUB-BASE]
sub (TBase v1 b1 (Known p1)) (TBase v2 b2 (Known p2))
  | b1 == b2  = return $ CAll v1 b1 p1 (CPred $ substPred v1 v2 p2)
  | otherwise = failWith $ InvalidSubtypingBase b1 b2

-- [SUB-FUN]
sub (TFun x1 s1 t1) (TFun x2 s2 t2) = do
  cI <- sub s2 s1
  let t1' = substType x1 x2 t1
  cO <- implCon x2 s2 <$> sub t1' t2
  return $ CConj cI cO

sub t1 t2 = failWith $ InvalidSubtyping t1 t2

-- | Implication constraint @(x :: t) => c@.
implCon :: Name -> Type -> Con -> Con
implCon x t c = case t of
  TBase v b (Known p) -> CAll x b (substPred x v p) c
  _                   -> c


------------------------------------------------------------------------------
-- Substitution

-- | Regular name substitution inside predicates. Note that the predicate
-- language does not have binders, so we don't need to worry about name capture.
--
-- @substPred x y p  ===  p[x/y]  ===  p where x replaces y@
substPred :: Name -> Name -> Pred -> Pred
substPred x y = go
 where
  go PTrue = PTrue
  go PFalse = PFalse    
  go (PVar n) = if n == y then PVar x else PVar n
  go (PBin o p1 p2) = PBin o (go p1) (go p2)
  go (PRel r p1 p2) = PRel r (go p1) (go p2)
  go (PConj p1 p2) = PConj (go p1) (go p2)
  go (PDisj p1 p2) = PDisj (go p1) (go p2)
  go (PImpl p1 p2) = PImpl (go p1) (go p2)
  go (PIff p1 p2) = PIff (go p1) (go p2)
  go (PNot p1) = PNot (go p1)
  go (PFun f ps) = PFun f (map go ps)

substReft :: Name -> Name -> Reft -> Reft
substReft x y = \case
  Unknown -> Unknown
  Known p -> Known (substPred x y p)

-- | Capture-avoiding substitution inside types.
--
-- @substType x y p  ===  p[x/y]  ===  p where x replaces y@
substType :: Name -> Name -> Type -> Type
substType x y = go
 where
  -- In a refined base type {n:b|r}, the value variable n names the
  -- value of type b that is being refined. In a sense, n is bound in r.
  go (TBase n b r)
    -- 1. If the bound name n is the same as the name y that we are replacing,
    --    then there can be no free occurrences of y in r.
    | n == y = TBase n b r

    -- 2. If the bound name n is the same as the substitution x, then we need to
    --    rename n to something fresh that doesn't yet occur in r.
    | n == x = let n' = freshName x (y : freeVarsInReft r)
                   r' = substReft n' n r
               in TBase n' b r'
      
    -- 3. If the bound name n is neither x nor y, we can just recurse.
    | otherwise = TBase n b (substReft x y r)

  -- In a dependent function type n:t1 -> t2, the name n binds t1 in t2. 
  -- Note that t1 might itself contain (free) occurrences of n.
  go (TFun n t1 t2)

    -- 1. If the bound name n is the same as the name y that we are replacing,
    --    then there can be no free occurrences of y in t2. However, there could
    --    be free occurrences of y in t1.
    | n == y = TFun n (go t1) t2
    
    -- 2. If the bound name n is the same as the substitution x, then we need to
    --    rename n to something fresh that doesn't yet occur in t2.
    | n == x = let n'  = freshName x (y : freeVarsInType t2)
                   t2' = substType n' n t2
               in TFun n' (go t1) (go t2')
    
    -- 3. If the bound name n is neither x nor y, we can just recurse.
    | otherwise = TFun n (go t1) (go t2)

freshName :: Name -> [Name] -> Name
freshName x ys = go (nextSubscript x)
 where
  go x'
    | x' `elem` ys = go (nextSubscript x')
    | otherwise    = x'

freeVarsInType :: Type -> [Name]
freeVarsInType = \case
  TBase v b r -> freeVarsInReft r \\ [v]
  TFun x t1 t2 -> freeVarsInType t1 ++ (freeVarsInType t2 \\ [x])

freeVarsInReft :: Reft -> [Name]
freeVarsInReft = \case
  Unknown -> []
  Known p -> freeVarsInPred p

freeVarsInPred :: Pred -> [Name]
freeVarsInPred = \case
  PTrue -> []
  PFalse -> []
  PVar n -> [n]
  PBin o p1 p2 -> freeVarsInPred p1 ++ freeVarsInPred p2
  PRel r p1 p2 -> freeVarsInPred p1 ++ freeVarsInPred p2
  PConj p1 p2 -> freeVarsInPred p1 ++ freeVarsInPred p2
  PDisj p1 p2 -> freeVarsInPred p1 ++ freeVarsInPred p2
  PImpl p1 p2 -> freeVarsInPred p1 ++ freeVarsInPred p2
  PIff p1 p2 -> freeVarsInPred p1 ++ freeVarsInPred p2
  PNot p1 -> freeVarsInPred p1
  PFun f ps -> concatMap freeVarsInPred ps
