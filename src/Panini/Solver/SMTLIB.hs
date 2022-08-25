-- | Conversion of predicate logic constraints (`Con`) to SMT-LIB syntax. See
-- https://smtlib.cs.uiowa.edu for more information about the output format.
module Panini.Solver.SMTLIB where

import Data.Text (Text)
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder qualified as LB
import Panini.Syntax
import Prelude

------------------------------------------------------------------------------

printSMTLib2 :: SMTLib2 a => a -> Text
printSMTLib2 = LT.toStrict . LB.toLazyText . encode

------------------------------------------------------------------------------

class SMTLib2 a where
  encode :: a -> Builder

parens :: Builder -> Builder
parens a = "(" <> a <> ")"

(<+>) :: Builder -> Builder -> Builder
(<+>) a b = a <> " " <> b

sexpr :: [Builder] -> Builder
sexpr  = parens . foldr1 (<+>)

instance SMTLib2 Con where
  encode (CHead p) = encode p
  encode (CAnd c1 c2) = sexpr ["and", encode c1, encode c2]
  encode (CAll x b p c) = sexpr ["forall", sexpr [sort], impl]
    where
      sort = sexpr [encode x, encode b]
      impl = sexpr ["=>", encode p, encode c]

instance SMTLib2 Pred where
  encode (PRel r e1 e2) = sexpr [encode r, encode e1, encode e2]
  encode (PAnd ps) = sexpr ("and" : map encode ps)
  encode (POr ps) = sexpr ("or" : map encode ps)
  encode (PImpl p1 p2) = sexpr ["=>", encode p1, encode p2]
  encode (PIff p1 p2) = sexpr ["iff", encode p1, encode p2]
  encode (PNot p) = sexpr ["not", encode p]
  encode (PAppK k xs) = sexpr (encode k : map encode xs)
  encode (PExists x b p) = sexpr ["exists", sexpr [sort], encode p]
    where
      sort = sexpr [encode x, encode b]
  encode PTrue = sexpr ["true"]
  encode PFalse = sexpr ["false"]

instance SMTLib2 PExpr where
  encode (PVar n) = encode n
  encode (PCon c) = encode c
  encode (PBin o e1 e2) = sexpr [encode o, encode e1, encode e2]
  encode (PFun x ps) = sexpr (encode x : map encode ps)
  encode (PStrLen p) = sexpr ["str.len", encode p]
  encode (PStrAt p1 p2) = sexpr ["str.at", encode p1, encode p2]
  encode (PStrSub p1 p2 p3) = encodeSubstring p1 p2 p3

-- NB: we represent the substring operation using [start..end] ranges,
-- but SMTLIB/Z3Str expects start plus length, so we have to convert
encodeSubstring :: PExpr -> PExpr -> PExpr -> Builder
encodeSubstring p1 p2 p3 = 
  sexpr ["str.substr", encode p1, encode p2, encode offset]
  where
    offset = case (p2,p3) of
      (PCon (I i _), PCon (I j _)) -> PCon (I (j - i) NoPV)
      _                            -> PBin Sub p3 p2

-- TODO: would kvars ever even be part of something sent to the solver?
-- TODO: ensure uniqueness
instance SMTLib2 KVar where
  encode (KVar i _) = "k" <> LB.fromString (show i)

instance SMTLib2 Name where
  encode (Name n _) = LB.fromText n

instance SMTLib2 Constant where
  encode (U _) = "0"  -- TODO
  encode (B True _) = "true"
  encode (B False _) = "false"
  encode (I x _) = LB.fromString (show x)
  encode (S x _) = LB.fromString (show x)

instance SMTLib2 Bop where
  encode Add = "+"
  encode Sub = "-"
  encode Mul = "*"
  encode Div = "/"

instance SMTLib2 Rel where
  encode Eq = "="
  encode Ne = "disequal"
  encode Ge = ">="
  encode Le = "<="
  encode Gt = ">"
  encode Lt = "<"

instance SMTLib2 Base where
  encode TUnit = "Int"  -- TODO
  encode TBool = "Bool"
  encode TInt = "Int"
  encode TString = "String"
