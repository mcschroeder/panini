-- | Conversion of predicate logic constraints (`Con`) to SMT-LIB syntax. See
-- https://smtlib.cs.uiowa.edu for more information about the output format.
module Panini.SMT.SMTLIB (SMTLIB(..), toSMTLIB) where

import Data.Text (Text)
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder qualified as LB
import Panini.Logic.Constraints
import Panini.Logic.Expressions
import Panini.Logic.KVar
import Panini.Logic.Predicates
import Panini.Logic.Relations
import Panini.Names
import Panini.Primitives
import Panini.Provenance
import Prelude

------------------------------------------------------------------------------

class SMTLIB a where
  encode :: a -> Builder

toSMTLIB :: SMTLIB a => a -> Text
toSMTLIB = LT.toStrict . LB.toLazyText . encode

------------------------------------------------------------------------------

sexpr :: [Builder] -> Builder
sexpr xs = "(" <> foldr1 (\a b -> a <> " " <> b) xs <> ")"

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
    PRel p       -> encode p
    PAppK k xs    -> sexpr (encode k : map encode xs)
    PExists x b p -> sexpr ["exists", sorts [(x,b)], encode p]
    

instance SMTLIB Rel where
  encode = \case
    Rel r e1 e2  -> sexpr [encode r, encode e1, encode e2]


instance SMTLIB Expr where
  encode = \case
    EVal v           -> encode v
    EAdd e1 e2       -> sexpr ["+", encode e1, encode e2]
    ESub e1 e2       -> sexpr ["-", encode e1, encode e2]
    EMul e1 e2       -> sexpr ["*", encode e1, encode e2]
    EFun x ps        -> sexpr (encode x : map encode ps)
    EStrLen p        -> sexpr ["str.len", encode p]
    EStrAt p1 p2     -> sexpr ["str.at", encode p1, encode p2]
    EStrSub p1 p2 p3 -> encodeSubstring p1 p2 p3
    EAbs _ -> undefined -- TODO
    ENot _ -> undefined -- TODO

-- NB: we represent the substring operation using [start..end] ranges,
-- but SMTLIB/Z3Str expects start plus length, so we have to convert
encodeSubstring :: Expr -> Expr -> Expr -> Builder
encodeSubstring p1 p2 p3 = 
  sexpr ["str.substr", encode p1, encode p2, encode offset]
  where
    offset = case (p2,p3) of
      (ECon (I i _), ECon (I j _)) -> ECon (I (j - i) NoPV)
      _                            -> ESub p3 p2

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
    I x     _ -> LB.fromString (show x)
    S x     _ -> LB.fromString (show x)

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
