-- | Conversion of predicate logic constraints (`Con`) to SMT-LIB syntax. See
-- https://smtlib.cs.uiowa.edu for more information about the output format.
module Panini.SMT.SMTLIB (SMTLIB(..), toSMTLIB) where

import Data.Text (Text)
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder qualified as LB
import Panini.Abstract.AExpr
import Panini.Abstract.AString (toRegLan)
import Panini.Pretty
import Panini.Provenance
import Panini.SMT.RegLan
import Panini.Solver.Constraints
import Panini.Syntax
import Prelude

-- TODO: ensure variable names are encoded properly (e.g., n')

------------------------------------------------------------------------------

class SMTLIB a where
  encode :: a -> Builder

toSMTLIB :: SMTLIB a => a -> Text
toSMTLIB = LT.toStrict . LB.toLazyText . encode

------------------------------------------------------------------------------

sexpr :: [Builder] -> Builder
sexpr xs = "(" <> foldr1 (\a b -> a <> " " <> b) xs <> ")"

fromShow :: Show a => a -> Builder
fromShow = LB.fromString . show

sorts :: [(Name, Base)] -> Builder
sorts = sexpr . map (\(x,b) -> sexpr [encode x, encode b])

instance SMTLIB Con where
  encode = \case
    CHead p      -> encode p
    CAnd c1 c2   -> sexpr ["and", encode c1, encode c2]
    CAll x b p c -> sexpr [ "forall", sorts [(x,b)]
                          , sexpr ["=>", encode p, encode c]
                          ]

instance SMTLIB FlatCon where
  encode (FAll [] p q) = sexpr ["=>", encode p, encode q]
  encode (FAll xs p q) = sexpr [ "forall", sorts xs
                               , sexpr ["=>", encode p, encode q]]

instance SMTLIB Pred where
  encode = \case
    PTrue         -> "true"
    PFalse        -> "false"
    PAnd ps       -> sexpr ("and" : map encode ps)
    POr ps        -> sexpr ("or" : map encode ps)
    PImpl p1 p2   -> sexpr ["=>", encode p1, encode p2]
    PIff p1 p2    -> sexpr ["iff", encode p1, encode p2]
    PNot p        -> sexpr ["not", encode p]
    PRel p        -> encode p
    PAppK k xs    -> sexpr (encode k : map encode xs)
    PExists x b p -> sexpr ["exists", sorts [(x,b)], encode p]
    

instance SMTLIB Rel where
  encode = \case
    Rel In e1 (EStrA r) -> sexpr ["str.in.re", encode e1, encode (toRegLan r)]
    Rel r e1 e2  -> sexpr [encode r, encode e1, encode e2]

instance SMTLIB RegLan where
  encode = \case
    ToRe s    -> sexpr ["str.to.re", fromShow s]
    None      -> "re.none"
    All       -> "re.all"
    AllChar   -> "re.allchar"    
    Conc a b  -> sexpr ["re.++", encode a, encode b]
    Union a b -> sexpr ["re.union", encode a, encode b]
    Inter a b -> sexpr ["re.inter", encode a, encode b]
    Star a    -> sexpr ["re.*", encode a]
    Comp a    -> sexpr ["re.comp", encode a]
    Diff a b  -> sexpr ["re.diff", encode a, encode b]
    Plus a    -> sexpr ["re.+", encode a]    
    Opt a     -> sexpr ["re.opt", encode a]
    Range a b -> sexpr ["re.range", fromShow [a], fromShow [b]]

instance SMTLIB Expr where
  encode = \case
    EVal v           -> encode v
    ENot e           -> sexpr ["not", encode e]
    EAdd e1 e2       -> sexpr ["+", encode e1, encode e2]
    ESub e1 e2       -> sexpr ["-", encode e1, encode e2]
    EMul e1 e2       -> sexpr ["*", encode e1, encode e2]
    EFun x ps        -> sexpr (encode x : map encode ps)
    EStrLen p        -> sexpr ["str.len", encode p]
    EStrAt p1 p2     -> sexpr ["str.at", encode p1, encode p2]
    EStrSub p1 p2 p3 -> encodeSubstring p1 p2 p3    
    EAbs a -> error $ "SMTLIB: unencondable abstract value: " ++ showPretty a

-- NB: we represent the substring operation using [start..end] ranges,
-- but SMTLIB/Z3Str expects start plus length, so we have to convert
encodeSubstring :: Expr -> Expr -> Expr -> Builder
encodeSubstring p1 p2 p3 = 
  sexpr ["str.substr", encode p1, encode p2, encode offset]
  where
    offset = case (p2,p3) of
      (ECon (I i _), ECon (I j _)) -> ECon (I (j - i + 1) NoPV)
      _                            -> p3 :-: p2 :+: (EInt 1 NoPV)

-- TODO: ensure uniqueness
instance SMTLIB KVar where
  encode (KVar i _) = "k" <> LB.fromString (show i)

instance SMTLIB Value where
  encode (Var x) = encode x
  encode (Con c) = encode c

instance SMTLIB Name where
  encode (Name n _) = LB.fromText n

instance SMTLIB Constant where
  encode = \case
    U       _ -> "0"  -- TODO
    B True  _ -> "true"
    B False _ -> "false"
    I x     _ -> fromShow x
    S x     _ -> fromShow x

instance SMTLIB Rop where
  encode = \case
    Eq -> "="
    Ne -> "distinct"
    Ge -> ">="
    Le -> "<="
    Gt -> ">"
    Lt -> "<"
    In -> undefined -- TODO
    Ni -> undefined -- TODO

instance SMTLIB Base where
  encode = \case
    TUnit   -> "Int"  -- TODO
    TBool   -> "Bool"
    TInt    -> "Int"
    TString -> "String"
