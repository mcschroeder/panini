module FuzzingBook where

import Control.Monad.Trans.State.Strict
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Foldable1
import Data.IntSet qualified as IntSet
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.String
import Data.Text qualified as Text
import Regex.CharSet qualified as CS
import Regex.Type
import Data.List
import Data.Function
import Data.Set qualified as Set

-------------------------------------------------------------------------------

-- | The grammar format of <https://www.fuzzingbook.org>.
newtype Grammar = Grammar (Map Ident Alt)

data Alt = Alt (NonEmpty Con)   -- alternations of concatenations
  deriving stock (Eq, Ord)

data Con = Con (NonEmpty Sym)   -- concatentations of symbols
  deriving stock (Eq, Ord)

data Sym = T String | NT Ident  -- terminal or non-terminal symbol
  deriving stock (Eq, Ord)

newtype Ident = Ident String
  deriving newtype (Eq, Ord)

startSymbol :: Ident
startSymbol = Ident "start"

-------------------------------------------------------------------------------

printGrammar :: Grammar -> String
printGrammar = BS.unpack . encode

instance ToJSON Ident where
  toJSON (Ident s) = String $ Text.pack $ "<" ++ s ++ ">"

instance ToJSONKey Ident where
  toJSONKey = toJSONKeyText (\(Ident s) -> Text.pack $ "<" ++ s ++ ">")

instance ToJSON Sym where
  toJSON (T t) = String $ Text.pack t
  toJSON (NT nt) = toJSON nt

instance ToJSON Alt where
  toJSON (Alt xs) = toJSON xs

instance ToJSON Con where
  toJSON (Con xs) = toJSON xs

instance ToJSON Grammar where
  toJSON (Grammar g) = toJSON g

-------------------------------------------------------------------------------

fromRegex :: Regex -> Grammar
fromRegex r0 = inline 
             $ removeDuplicates 
             $ fuseCons 
             $ fst 
             $ execState goStart (Grammar Map.empty, 0)
 where
  goStart :: State (Grammar,Int) ()
  goStart = do
    n1 <- go r0
    addRule startSymbol (Alt $ NE.singleton $ Con $ NE.singleton $ NT n1)

  go :: Regex -> State (Grammar,Int) Ident
  go = \case
    Zero -> error "FuzzingBookGrammar.fromRegex: ∅ unsupported"    
    One -> newRule (Alt $ NE.singleton eps)

    Lit cs -> case CS.fromCharSet cs of
      (True, xs) -> do
        let cs = map (toEnum @Char) $ IntSet.toAscList xs
        let cons = map (Con . NE.singleton . T . pure) cs
        newRule (Alt $ NE.fromList cons)
      (False, xs) -> do
        let cs = map (toEnum @Char) $ IntSet.toAscList xs
        newRule (Alt $ allASCIIExcept cs)

    Plus rs -> do      
      cons <- map (Con . NE.singleton . NT) <$> mapM go rs
      newRule (Alt $ NE.fromList cons)

    Times rs -> do
      ns <- mapM go rs
      let con = Con $ NE.fromList $ map NT ns
      newRule (Alt $ NE.singleton con)

    Star r -> do
      n1 <- newNT
      n2 <- go r
      let con_rec = Con $ NE.fromList [NT n2, NT n1]
      addRule n1 (Alt $ NE.fromList [eps, con_rec])
      return n1
    
    Opt r -> do
      n2 <- go r
      let con_sym = Con $ NE.fromList [NT n2]
      newRule (Alt $ NE.fromList [eps, con_sym])

  newRule :: Alt -> State (Grammar,Int) Ident
  newRule xs = newNT >>= \n1 -> addRule n1 xs >> return n1

  addRule :: Ident -> Alt -> State (Grammar, Int) ()
  addRule nt xs = do
    (Grammar g, i) <- get
    let g' = Map.insert nt xs g
    put (Grammar g', i)
  
  newNT :: State (Grammar, Int) Ident
  newNT = do
    (g,i) <- get
    put (g, i + 1)
    return $ Ident $ show i

  eps :: Con
  eps = Con $ NE.singleton $ T ""

  allASCIIExcept :: [Char] -> NonEmpty Con
  allASCIIExcept xs = NE.fromList 
                    $ map (Con . NE.singleton . T . pure)
                    $ filter (\x -> x `notElem` xs)
                    $ map toEnum [0..127]

fuseCons :: Grammar -> Grammar
fuseCons (Grammar g) = Grammar $ Map.map goAlt g
 where
  goAlt (Alt xs) = Alt (NE.map goCon xs)
  goCon (Con xs) = Con (NE.fromList $ go $ NE.toList xs)
  go (T x : T y : zs) = T (x ++ y) : go zs
  go (x:xs) = x : go xs
  go [] = []

removeDuplicates :: Grammar -> Grammar
removeDuplicates (Grammar g0) = 
  removeDups $ foldr replace1 (Grammar g0) dups
 where
  groups = groupBy ((==) `on` snd) $ sortBy (compare `on` snd) $ Map.toList g0
  dups = [ (k,ks) | grp <- groups, length grp > 1, let (k:ks) = map fst grp ]  
  replace1 (new,olds) g = foldr (\old g -> replaceNT old (NT new) g) g olds
  rems = Set.fromList $ concatMap snd dups
  removeDups (Grammar g) = Grammar $ Map.withoutKeys g rems

replaceNT :: Ident -> Sym -> Grammar -> Grammar
replaceNT old new (Grammar g) = Grammar $ Map.map goAlt g
 where
  goAlt (Alt xs) = Alt $ NE.map goCon xs
  goCon (Con xs) = Con $ NE.map goSym xs
  goSym (NT x) | x == old = new
  goSym x = x

inline :: Grammar -> Grammar
inline (Grammar g0) = 
  removeDead $ foldr replace1 (Grammar g0) simps
 where
  simps = [ (k,t) | (k, Alt (Con (T t :| []) :| [])) <- Map.toList g0]
  replace1 (k,t) g = replaceNT k (T t) g
  dead = Set.fromList $ map fst simps
  removeDead (Grammar g) = Grammar $ Map.withoutKeys g dead
