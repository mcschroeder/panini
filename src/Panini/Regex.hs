-- | This module contains types and functions to work with extended regular
-- expressions (i.e., regexes that permit intersection and complement).
--
-- There are some aspects of note:
--
--   1) The constructors of the 'Regex' data type are somewhat optimized for
--      efficiency of representation and thus not mathematically "minimal". For
--      example, choice (+) and sequence (⋅) are n-ary operations, and we
--      include a redundant constructor for optionals (?).
--
--   2) The literals in the 'Regex' data type are character sets ('AChar')
--      instead of just single characters ('Char'). This enables efficient and
--      succinct representation of character classes (e.g., @[a-z]@).
--
--   3) Operations like 'intersection' and 'normalize' are implemented entirely
--      algebraically, without intermediate translation into automata.
--
-- References:
--
--   * Acay, Josh. "A Regular Expression Library for Haskell." Unpublished
--     manuscript, dated May 22, 2018. LaTeX files and Haskell source code.
--     https://github.com/cacay/regexp
--
--   * Antimirov, Valentin. 1996. "Partial derivatives of regular expressions
--     and finite automaton constructions." Theoretical Computer Science 155
--     (1996): 291-319. https://doi.org/10.1016/0304-3975(95)00182-4
--
--   * Arden, Dean N. 1961. "Delayed Logic and Finite State Machines."
--     Proceedings of the 2nd Annual Symposium on Switching Circuit Theory and
--     Logical Design (SWCT 1961), 133-151. https://doi.org/10.1109/FOCS.1961.13
--
--   * Brzozowski, Janusz A. 1964. "Derivatives of Regular Expressions." Journal
--     of the ACM 11, no. 4 (October 1964): 481-494.
--     https://doi.org/10.1145/321239.321249
--
--   * Keil, Matthias and Peter Thiemann. 2014. "Symbolic Solving of Extended
--     Regular Expression Inequalities." https://arxiv.org/abs/1410.3227
--
--   * Liang Tianyi, Nestan Tsiskaridze, Andrew Reynolds, Cesare Tinelli, and
--     Clark Barrett. 2015. "A decision procedure for regular membership and
--     length constraints over unbounded strings." Frontiers of Combining
--     Systems (FroCoS 2015), LNAI 9322, 135–150.
--     https://doi.org/10.1007/978-3-319-24246-0_9
--
module Panini.Regex where

import Algebra.Lattice
import Control.Exception
import Control.Monad.Trans.State.Strict
import Data.Containers.ListUtils
import Data.Generics.Uniplate.Direct
import Data.Hashable
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Semigroup hiding (All)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String
import GHC.Generics
import Panini.Abstract.AChar (AChar)
import Panini.Abstract.AChar qualified as AChar
import Panini.Pretty
import Prelude

-------------------------------------------------------------------------------

-- TODO
type CharSet = AChar

-------------------------------------------------------------------------------

-- TODO: simplification vs standardization: simplification is needed for
-- succinctness (human readability); standardization is needed for correctness
-- of algorithm (so that intermediate expressions don't blow up infinitely but
-- instead collapse to unique representations)


-- | The 'Regex' type defines regular expressions over Unicode characters.
data Regex  
  = Lit CharSet   -- ^ set of literal symbols, character class
  | Word String   -- ^ word, sequence of singleton literals
  | Plus [Regex]  -- ^ choice (r₁ + r₂), alternation (r₁ | r₂), join (r₁ ∨ r₂)
  | Times [Regex] -- ^ sequence (r₁ ⋅ r₂), concatenation (r₁ <> r₂)
  | Star Regex    -- ^ iteration, Kleene closure (r*)
  | Opt Regex     -- ^ option (r?)
  deriving stock (Eq, Ord, Show, Read, Generic)
-- TODO: n-times repetition
-- TODO: Plus (+)

-- | zero element (0), empty set (∅), bottom (⊥)
pattern Zero :: Regex
pattern Zero <- Lit (isBot -> True) where
  Zero = Lit bot

-- | identitity element (1), empty string (ε)
pattern One :: Regex
pattern One = Word ""

-- | set of all singleton words (Σ), class of all characters
pattern AnyChar :: Regex
pattern AnyChar <- Lit (isTop -> True) where
  AnyChar = Lit top

-- | set of all words (Σ*), top (⊤)
pattern All :: Regex
pattern All = Star AnyChar

instance Hashable Regex

instance IsString Regex where
  fromString = Word

instance Semigroup Regex where
  Times xs <> Times ys = Times (xs ++ ys)
  Times xs <> r        = Times (xs ++ [r])
  r        <> Times xs = Times (r:xs)
  r1       <> r2       = Times [r1,r2]

  stimes 0 _ = One
  stimes 1 r = r
  stimes n r = foldr1 (<>) $ replicate (fromIntegral n) r

instance Monoid Regex where
  mempty = One

instance JoinSemilattice Regex where
  Plus rs1 ∨ Plus rs2 = Plus (rs1 ++ rs2)
  Plus rs  ∨ r        = Plus (rs ++ [r])
  r        ∨ Plus rs  = Plus (r:rs)
  r1       ∨ r2       = Plus [r1,r2]

instance BoundedJoinSemilattice Regex where
  bot = Zero

instance MeetSemilattice Regex where
  r1 ∧ r2 = simplify $ intersection r1 r2

instance BoundedMeetSemilattice Regex where
  top = All

instance ComplementedLattice Regex where
  neg = simplify . complement

-------------------------------------------------------------------------------

instance Pretty Regex where
  pretty = go True
   where
    go o = \case
      Lit c -> pretty c
      Word [] -> epsilon
      Word s -> pretty s
      Plus rs -> parensIf o $ concatWithOp "+" $ map (go False) rs
      Times rs -> mconcat $ map (go True) rs
      Star r@(Lit _) -> pretty r <> "*"
      Star r -> parens (go False r) <> "*"
      Opt r -> parens (go False r) <> "?"

instance Uniplate Regex where
  uniplate = \case
    Lit c    -> plate Lit |- c
    Word s   -> plate Word |- s
    Plus rs  -> plate Plus ||* rs
    Times rs -> plate Times ||* rs
    Star r   -> plate Star |* r
    Opt r    -> plate Opt |* r

-------------------------------------------------------------------------------

-- | A regex is /nullable/ if it accepts the empty string.
nullable :: Regex -> Bool
nullable = \case
  Lit _    -> False
  Word s   -> null s
  Plus rs  -> or $ map nullable rs
  Times rs -> and $ map nullable rs
  Star _   -> True
  Opt _    -> True

-------------------------------------------------------------------------------

-- TODO: implement more complex simplifications à la Kahrs/Runciman 2022

simplify :: Regex -> Regex
simplify = rewrite $ \case
  Lit a | [c] <- AChar.values a -> Just $ Word [c]

  Plus rs0 -> case filter (/= Zero) $ nubOrd rs0 of
    []                             -> Just Zero
    [r]                            -> Just r
    rs1 | any (== One) rs1         -> Just $ Opt $ Plus $ filter (/= One) rs1
        | any isOpt rs1            -> Just $ Opt $ Plus $ concatMap flatOpt rs1
        | all isLit rs1            -> Just $ Lit $ joins [a | Lit a <- rs1]
        | all isWord1 rs1          -> Just $ Lit $ joins [AChar.eq c | Word [c] <- rs1]
        | any isPlus rs1           -> Just $ Plus $ concatMap flatPlus rs1
        | length rs1 /= length rs0 -> Just $ Plus rs1
        | otherwise                -> Nothing

  Times rs0 -> case filter (/= One) rs0 of
    []                             -> Just One
    [r]                            -> Just r
    rs1 | any (== Zero) rs1        -> Just Zero
        | all isWord rs1           -> Just $ Word $ concat [s | Word s <- rs1]
        | any isTimes rs1          -> Just $ Times $ concatMap flatTimes rs1
        | length rs1 /= length rs0 -> Just $ Times rs1
        | otherwise                -> Nothing

  Star Zero     -> Just One
  Star One      -> Just One
  Star (Star r) -> Just $ Star r
  
  Opt Zero           -> Just One
  Opt One            -> Just One
  Opt r | nullable r -> Just r

  _ -> Nothing

isLit :: Regex -> Bool
isLit (Lit _) = True
isLit _       = False
{-# INLINE isLit #-}

isWord :: Regex -> Bool
isWord (Word _) = True
isWord _        = False
{-# INLINE isWord #-}

isWord1 :: Regex -> Bool
isWord1 (Word [_]) = True
isWord1 _          = False
{-# INLINE isWord1 #-}

isTimes :: Regex -> Bool
isTimes (Times _) = True
isTimes _         = False
{-# INLINE isTimes #-}

isPlus :: Regex -> Bool
isPlus (Plus _) = True
isPlus _        = False
{-# INLINE isPlus #-}

isOpt :: Regex -> Bool
isOpt (Opt _) = True
isOpt _       = False
{-# INLINE isOpt #-}

flatTimes :: Regex -> [Regex]
flatTimes (Times xs) = xs
flatTimes x          = [x]
{-# INLINE flatTimes #-}

flatPlus :: Regex -> [Regex]
flatPlus (Plus xs) = xs
flatPlus x         = [x]
{-# INLINE flatPlus #-}

flatOpt :: Regex -> [Regex]
flatOpt (Opt r) = [r]
flatOpt x       = [x]
{-# INLINE flatOpt #-}

-------------------------------------------------------------------------------

-- | Compute the intersection of two regexes.
--
-- The implementation works purely algebraically, without going through a DFA.
-- It is based on the equation-solving approach by Acay (unpublished manuscript,
-- 2018), using Arden's lemma (1961) and Gaussian elimination to solve a system
-- of regex equations. The approach is similar to the one by Liang et al. (2015,
-- Fig. 8), where the interplay of the π and ρ functions essentially does the
-- same thing. Like Acay, we also make use of the local mintermization approach
-- by Keil and Thiemann (2014) to effectively compute precise derivatives over
-- large alphabets (see the 'next' function below).
intersection :: Regex -> Regex -> Regex
intersection = curry $ solve $ \(r1,r2) ->
  let c0 = if nullable r1 && nullable r2 then One else Zero
      cx = [ ((simplify $ derivative c r1, simplify $ derivative c r2), Lit p) 
           | p <- Set.toList $ next r1 ⋈ next r2
           , Just c <- [AChar.choose p]
           ]
  in (c0, Map.fromList cx)

-- | Compute the complement of a regex.
--
-- See the 'intersection' operation for notes on the implementation.
complement :: Regex -> Regex
complement = solve $ \r ->
  let c0 = if nullable r then Zero else One
      c1 = Lit (neg $ joins $ next r) <> All
      cx = [ (simplify $ derivative c r, Lit p)
           | p <- Set.toList $ next r
           , Just c <- [AChar.choose p]
           ]
  in (c0 ∨ c1, Map.fromList cx)

-------------------------------------------------------------------------------

-- | A system of regex equations.
type System x = Map x (RHS x)

-- | The right-hand side of an equation @Xᵢ = c₀ + c₁⋅X₁ + c₂⋅X₂ + … + cₙXₙ@,
-- with @c₀@ being a known constant term. Each unknown term @c⋅X@, consisting of
-- a coefficient @c@ and a variable @X@, is represented as an entry in a map
-- from @X@ to @c@.
type RHS x = (Regex, Map x Regex)

-- | @solve f X₀@ dynamically constructs and solves a system of linear regex
-- equations, given an initial unknown variable @X₀@ and a function @f@ that
-- computes the right-hand side of any unknown variable.
solve :: forall x. Ord x => (x -> RHS x) -> x -> Regex
solve f x0 = evalState (go x0) mempty
 where
  go :: x -> State (System x) Regex
  go x = do
    s <- gets $ Map.lookup x
    case s of
      Just (c0,xs) ->
        assert (null xs)
        return c0

      Nothing -> do
        (c0,ys) <- elim x <$> resolve (f x)
        modify' $ Map.insert x (c0,ys)
        cs <- mapM (\(y,c) -> (c <>) <$> go y) (Map.toList ys)
        let c0' = joins (c0:cs)
        modify' $ Map.insert x (c0', mempty)
        return c0'

  resolve :: RHS x -> State (System x) (RHS x)
  resolve (c0,xs) = do
    rs <- mapM resolveTerm (Map.toList xs)
    return $ foldr combine (c0, mempty) rs

  resolveTerm :: (x, Regex) -> State (System x) (RHS x)
  resolveTerm (x,c) = do
    s <- gets $ Map.lookup x
    case s of
      Just (c0,xs) -> scale c <$> resolve (c0,xs)
      _            -> return (Zero, Map.singleton x c)

  elim :: x -> RHS x -> RHS x
  elim x (c0,xs) = 
    case Map.lookup x xs of
      Nothing -> (c0,xs)
      Just cx -> assert (not $ nullable cx)
                 scale (Star cx) (c0, Map.delete x xs)

  scale :: Regex -> RHS x -> RHS x
  scale r (c0,xs) = (r <> c0, Map.map (r <>) xs)

  combine :: RHS x -> RHS x -> RHS x
  combine (c01,xs1) (c02,xs2) = (c01 ∨ c02, Map.unionWith (∨) xs1 xs2)

-------------------------------------------------------------------------------

-- | The derivative c⁻¹r of a regex r with respect to a character c is a new
-- regex that accepts all words that would be accepted by r if they were
-- prefixed by c, i.e., ℒ(c⁻¹r) = { w | cw ∈ ℒ(r) }.
--
-- Regular expression derivatives were first introduced by Brzozowski (1964).
-- The notation c⁻¹ is due to Antimirov (1996), who also introduced the notion
-- of /partial/ derivatives. Note that in the literature, the partial derivative
-- operator ∂ is sometimes used to denote (non-partial) Brzozowski derivatives.
-- Both Keil and Thiemann (2014) and Liang et al. (2015) make this mistake, with
-- the latter even erroneously claiming to define the partial derivative
-- function while giving the classic Brzozowski definition (Fig. 6).
derivative :: Char -> Regex -> Regex
derivative c = \case
  Lit d 
    | AChar.member c d -> One
    | otherwise     -> Zero
  Word []           -> Zero
  Word (x:xs) 
    | c == x        -> Word xs
    | otherwise     -> Zero
  Plus []           -> Zero
  Plus [r]          -> derivative c r
  Plus rs           -> Plus $ map (derivative c) rs
  Times []          -> Zero
  Times [r]         -> derivative c r
  Times (r:rs) 
    | nullable r    -> Plus [Times (derivative c r : rs), derivative c (Times rs)]
    | otherwise     -> (derivative c r) <> Times rs
  Star r            -> (derivative c r) <> Star r
  Opt r             -> derivative c r

-------------------------------------------------------------------------------

-- | The  /next literals/ of a regex are a set {A₁,A₂,...,Aₙ} of mutually
-- disjoint character sets Aᵢ such that all symbols in each character set yield
-- the same derivative. This allows us to avoid enumerating the entire alphabet
-- during 'intersection': "[T]o determine a finite set of representatives for
-- all derivatives of a regular expression r it is sufficient to select one
-- symbol a from each equivalence class A ∈ next(r)∖{∅} and calculate ∂ₐ(r)."
-- (Keil and Thiemann 2014, section 5.2; note that ∂ here denotes the Brzozowski
-- derivative)
next :: Regex -> Set CharSet
next = \case
  Lit a          -> Set.singleton a
  Word []        -> Set.singleton bot
  Word (x:_)     -> Set.singleton (AChar.eq x)
  Plus []        -> Set.singleton bot
  Plus [r]       -> next r
  Plus (r:rs)    -> next r ⋈ next (Plus rs)
  Times []       -> Set.singleton bot
  Times [r]      -> next r
  Times (r:rs)
    | nullable r -> next r ⋈ next (Times rs)
    | otherwise  -> next r
  Star r         -> next r
  Opt r          -> next One ⋈ next r

-- | Given two sets of mutually disjoint literals, ⨝ (join) builds a new set of
-- mutually disjoint literals that covers the union of the two sets (Keil and
-- Thiemann 2014, Definition 7).
(⋈) :: Set CharSet -> Set CharSet -> Set CharSet
l1 ⋈ l2 = Set.fromList $ concat $
  [ [ a1 ∧ a2
    , a1 ∧ (neg $ joins l2)
    , a2 ∧ (neg $ joins l1)
    ]
  | a1 <- Set.toList l1, a2 <- Set.toList l2
  ]
