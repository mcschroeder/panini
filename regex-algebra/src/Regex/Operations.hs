{-|
This module implements 'intersection' and 'complement' of regular expressions
based on the equation-solving approach by Acay (unpublished manuscript, 2018),
which works entirely algebraically, without intermediate translation into
automata.

References:

  * Acay, Josh. "A Regular Expression Library for Haskell." Unpublished
    manuscript, dated May 22, 2018. LaTeX files and Haskell source code.
    https://github.com/cacay/regexp

  * Arden, Dean N. 1961. "Delayed Logic and Finite State Machines." Proceedings
    of the 2nd Annual Symposium on Switching Circuit Theory and Logical Design
    (SWCT 1961), 133-151. https://doi.org/10.1109/FOCS.1961.13

  * Keil, Matthias and Peter Thiemann. 2014. "Symbolic Solving of Extended
    Regular Expression Inequalities." https://arxiv.org/abs/1410.3227
    
-}
module Regex.Operations where

import Control.Applicative
import Control.Exception
import Control.Monad.Trans.State.Strict
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Semigroup hiding (All)
import Data.Set qualified as Set
import Prelude
import Regex.CharSet qualified as CS
import Regex.Derivative
import Regex.Type

-------------------------------------------------------------------------------

-- | Compute the intersection of two regexes.
--
-- The implementation works purely algebraically, without going through a DFA.
-- It is based on a definition of intersection via derivatives,
--
--     r₁ ∩ r₂ = c₀ + c₁⋅(c₁⁻¹r₁ ∩ c₁⁻¹r₂) + … + cₙ⋅(cₙ⁻¹r₁ ∩ cₙ⁻¹r₂),
--
-- where c₀ is ε if both r₁ and r₂ are nullable and otherwise ∅, and cᵢ
-- enumerates all characters of the alphabet Σ. Unfolding this definition gives
-- a system of linear equations
--
--     X₀ = c₀₀ + c₁₀⋅X₀ + c₂₀⋅X₁ + … + cₙ₀⋅Xₙ
--     X₁ = c₀₁ + c₁₁⋅X₀ + c₂₁⋅X₁ + … + cₙ₁⋅Xₙ
--        ⋮
--     Xₘ = c₀ₘ + c₁ₘ⋅X₀ + c₂ₘ⋅X₁ + … + cₙₘ⋅Xₙ
-- 
-- where each Xᵢ is an intersection of regex derivatives (cᵢₖ⁻¹r₁ ∩ cᵢₖ⁻¹r₂).
-- Using Arden's lemma (1961) and Gaussian elimination, we can solve this system
-- to arrive at a closed-form solution for X₀ = r₁ ∩ r₂.
--
-- To avoid actually enumerating the whole alphabet, we use local mintermization
-- (Keil and Thiemann 2014) to partition the alphabet into equivalence classes;
-- see the 'next' function below.
intersection :: Regex -> Regex -> Regex
intersection = curry $ solve $ \(x1,x2) ->
  let
    c0 | nullable x1, nullable x2 = One
       | otherwise                = Zero

    cx = [ (x, Lit p) | p <- Set.toList $ next x1 ⋈ next x2
                      , Just c <- [CS.choose p]
                      , let x = (derivative c x1, derivative c x2)
         ]

  in (c0, Map.fromListWith plus cx)

-- | Compute the complement of a regex.
--
-- The implementation works in the same way as 'intersection' and is based
-- on the definition
--
--     ¬r = c₀ + c₁⋅¬(c₁⁻¹r) + … + cₙ⋅¬(cₙ⁻¹r),
--
-- where c₀ is ε if r is not nullable and otherwise ε.
complement :: Regex -> Regex
complement r0 = fromMaybe (complement' r0) (lookupComplement r0)
 where
  complement' = solve $ \x1 ->
    let 
      c0 | nullable x1 = Zero 
         | otherwise  = One
    
      c1 = Lit (CS.complement $ CS.unions $ next x1) <> All
    
      cx = [ (x, Lit p) 
           | p      <- Set.toList $ next x1
           , Just c <- [CS.choose p]
           , let x   = derivative c x1
           ]

    in (c0 `plus` c1, Map.fromListWith plus cx)

-- TODO: move this into simplification module
-- | Look up known regex complements.
lookupComplement :: Regex -> Maybe Regex
lookupComplement = \case
  -- ¬(.*ab.*)  =  b*(a|[^ab]b*)*
  Times [All, a@(Lit a0), b@(Lit b0), All]
    | CS.intersection a0 b0 == CS.empty
    , let c = Lit (CS.complement (CS.union a0 b0))
    -> Just $ Star b <> Star (a `plus` (c <> Star b))
  
  -- ¬(.*aa.*)  =  (a?[^a])*a?
  Times [All, a@(Lit a0), a2, All]
    | a == a2
    , let ā = Lit (CS.complement a0)
    -> Just $ Star (Opt a <> ā) <> Opt a

  -- ¬(.*abb.*) = b*(ab?|[^ab]b*)*
  Times [All, a@(Lit a0), b@(Lit b0), b2, All]
    | b == b2
    , CS.intersection a0 b0 == CS.empty
    , let c = Lit (CS.complement (CS.union a0 b0))
    -> Just $ Star b <> Star ((a <> Opt b) `plus` (c <> Star b))
  
  -- ¬(b*(a|[^ab]b*)*)  =  .*ab.*
  Times1 (Star b) (Star (Plus1 a (Times1 c (Star b2))))
    | b == b2    
    , Lit a0 <- a
    , Lit b0 <- b
    , CS.intersection a0 b0 == CS.empty
    , Lit c0 <- c, c0 == CS.complement (CS.union a0 b0)    
    -> Just $ All <> a <> b <> All

  -- ¬((a?[^a])*a?)  =  .*aa.*
  Times1 (Star (Times1 (Opt a) (Lit ā))) (Opt a2)
    | a == a2
    , Lit a0 <- a
    , ā == CS.complement a0
    -> Just $ All <> a <> a <> All

  -- ¬(b*(ab?|[^ab]b*)*) = .*abb.*
  Times1 (Star b) (Star (Plus1 (Times1 a (Opt b2)) (Times1 c (Star b3))))
    | b == b2, b2 == b3
    , Lit a0 <- a
    , Lit b0 <- b
    , CS.intersection a0 b0 == CS.empty
    , Lit c0 <- c, c0 == CS.complement (CS.union a0 b0)
    -> Just $ All <> a <> b <> b <> All
  
  -- ¬(b*(ab?|[^ab]b*)*abb.*) = b*(ab?|[^ab]b*)*
  Times [Star b, r@(Star (Plus1 (Times1 c (Star b3)) (Times1 a (Opt b2)))), a2, b4, b5, All]
    | a == a2
    , b == b2, b2 == b3, b3 == b4, b4 == b5
    , Lit a0 <- a
    , Lit b0 <- b
    , CS.intersection a0 b0 == CS.empty
    , Lit c0 <- c, c0 == CS.complement (CS.union a0 b0)
    -> Just $ Star b <> r

  -- ¬((a?[^a])*a?aa)  =  .*aa.*aa|(a*[^a])*a?
  Times [Star (Times1 (Opt a) (Lit ā)), Opt a2, a3, a4]
    | a == a2, a2 == a3, a3 == a4
    , Lit a0 <- a
    , ā == CS.complement a0
    -> Just $ (All <> a <> a <> All <> a <> a) `plus` (Star (Star a <> Lit ā) <> Opt a)
    
  -- ¬((a?[^a])*a?aa.*)  =  (a?[^a])*a?
  Times [Star (Times1 (Opt a) (Lit ā)), Opt a2, a3, a4, All]
    | a == a2, a2 == a3, a3 == a4
    , Lit a0 <- a
    , ā == CS.complement a0
    -> Just $ Star (Opt a <> Lit ā) <> Opt a

  -- ¬(Σ*a)  =  (Σ*(Σ∖a))?
  Times [All, Lit a] 
    -> Just $ Opt (All <> Lit (CS.complement a))
  
  -- ¬(.*a.?)  =  ((.*[^a])?[^a])?
  Times [All, Lit a0, Opt AnyChar]
    | let ā = Lit (CS.complement a0)
    -> Just $ Opt (Opt (All <> ā) <> ā)
  
  -- ¬(((.*[^a])?[^a])?)  =  .*a.?
  Opt (Times1 (Opt (Times1 All (Lit ā0))) (Lit ā1))
    | ā0 == ā1
    , let a = Lit (CS.complement ā0)
    -> Just $ All <> a <> Opt AnyChar

  _ -> Nothing

-------------------------------------------------------------------------------

-- | A system of linear regex equations.
type System x = Map x (RHS x)

-- | The right-hand side of an equation Xᵢ = c₀ + c₁⋅X₁ + c₂⋅X₂ + … + cₙ⋅Xₙ,
-- with c₀ being a known constant term. Each unknown term c⋅X, consisting of a
-- coefficient c and a variable X, is represented as a mapping from X to c.
type RHS x = (Regex, Map x Regex)

-- | Combine two right-hand sides. For example,
--
--     combine (a + bX) (c + dX + eY)  =  (a+c) + (b+d)X + eY.
--
combine :: Ord x => RHS x -> RHS x -> RHS x
combine (c01,xs1) (c02,xs2) = (c01 `plus` c02, Map.unionWith plus xs1 xs2)

-- | Multiply each RHS term by a constant, i.e., concatenating the constant
-- regex in front of each term. For example,
-- 
--    scale c (a + bX)  =  ca + cbX.
--
scale :: Regex -> RHS x -> RHS x
scale r (c0,xs) = (r <> c0, Map.map (r <>) xs)

-- | Try to eliminate a variable using Arden's lemma (1961), which states that
--
--     X = A⋅X + B
--       = A*⋅B
-- 
-- as long as A is not nullable. For example,
--
--     elim X (a + bX + cY)  =  (b*a + b*cY).
--
elim :: Ord x => x -> RHS x -> RHS x
elim x (c0,xs) = case Map.lookup x xs of
  Nothing -> (c0,xs)  
  Just cx -> assert (not $ nullable cx)
             scale (Star cx) (c0, Map.delete x xs)

-- | @solve f X₁@ constructs and solves a system of linear regex equations,
-- given an initial unknown variable @X₁@ and a generating function @f@ that
-- computes the right-hand side of any unknown variable @Xᵢ@.
--
-- This implementation largely follows Acay (unpublished manuscript, 2018).
solve :: forall x. Ord x => (x -> RHS x) -> x -> Regex
solve f x0 = evalState (go x0) mempty
 where
  go :: x -> State (System x) Regex
  go x = do
    rhs <- gets $ Map.lookup x
    case rhs of
      Just (c0,xs) ->
        assert (null xs)
        return c0

      Nothing -> do
        -- generate the RHS for x and resolve all known variables
        rhsX <- resolve (f x)
        
        -- eliminate X using Arden's lemma and add the solution to the system
        let (c0,ys) = elim x rhsX
        modify' $ Map.insert x (c0,ys)

        -- recursively solve each remaining variable in the RHS
        cs <- mapM (\(y,c) -> (c <>) <$> go y) (Map.toList ys)
        
        -- add the final closed solution for X to the system and return it
        let c0' = Plus (c0:cs)
        modify' $ Map.insert x (c0', mempty)
        return c0'

  resolve :: RHS x -> State (System x) (RHS x)
  resolve (c0,xs) = do
    rs <- mapM resolveTerm (Map.toList xs)
    return $ foldr combine (c0, mempty) rs

  resolveTerm :: (x, Regex) -> State (System x) (RHS x)
  resolveTerm (x,c) = do
    rhs <- gets $ Map.lookup x
    case rhs of
      Just (c0,xs) -> scale c <$> resolve (c0,xs)
      Nothing      -> return (Zero, Map.singleton x c)
