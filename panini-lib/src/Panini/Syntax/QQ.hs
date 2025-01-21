{-# LANGUAGE TemplateHaskell #-}
module Panini.Syntax.QQ (panType, ρ, ω) where

import Data.Data
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String
import Data.Text qualified as Text
import Language.Haskell.TH (PatQ, varP, litP, listP, mkName, ExpQ)
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax qualified as TH
import Panini.Abstract.AChar (AChar)
import Panini.Abstract.AString qualified as AString
import Panini.Abstract.AValue
import Panini.Diagnostic
import Panini.Parser
import Panini.Pretty
import Panini.Syntax.Expressions
import Panini.Syntax.Names
import Panini.Syntax.Primitives
import Prelude

-------------------------------------------------------------------------------

panType :: QuasiQuoter
panType = QuasiQuoter {
  quoteExp = \s -> do
    case parseType "" (Text.strip $ Text.pack s) of
      Left e -> fail $ show $ group $ diagnosticMessage e
      Right ty -> dataToExpQ (const Nothing) ty
  
  , quotePat = undefined
  , quoteType = undefined
  , quoteDec = undefined
}

-------------------------------------------------------------------------------

-- TODO: document quasiquoter variable naming conventions other magic

ω :: QuasiQuoter
ω = QuasiQuoter
  { quoteExp = \s -> do
      case parseExpr "" (Text.strip $ Text.pack s) implicitTypes of
        Left e -> fail $ show $ group $ diagnosticMessage e
        Right p -> dataToExpQ (const Nothing `extQ` metaExp) (fmap fromValue p)

  , quotePat = \s -> do
      TH.putQ (mempty :: [Name])
      case parseExpr "" (Text.strip $ Text.pack s) implicitTypes of
        Left e -> fail $ show $ group $ diagnosticMessage e
        Right p -> dataToPatQ (const Nothing `extQ` metaPat) (fmap fromValue p)
  , quoteType = undefined
  , quoteDec = undefined
  }

ρ :: QuasiQuoter
ρ = QuasiQuoter 
  { quoteExp = \s -> do
      case parseRel "" (Text.strip $ Text.pack s) implicitTypes of
        Left e -> fail $ show $ group $ diagnosticMessage e
        Right p -> dataToExpQ (const Nothing `extQ` metaExp) (fmap fromValue p)

  , quotePat = \s -> do
      TH.putQ (mempty :: [Name])
      case parseRel "" (Text.strip $ Text.pack s) implicitTypes of
        Left e -> fail $ show $ group $ diagnosticMessage e
        Right p -> dataToPatQ (const Nothing `extQ` metaPat) (fmap fromValue p)
  , quoteType = undefined
  , quoteDec = undefined
  }

metaExp :: AExpr -> Maybe ExpQ
metaExp = \case
  EVar x _ | x `elem` genericMetaVars -> Just $ TH.varE $ liftName x
  EVar x TUnit   -> Just [| EVal (AUnit   $(TH.varE $ liftName x)) |]
  EVar x TBool   -> Just [| EVal (ABool   $(TH.varE $ liftName x)) |]
  EVar x TInt    -> Just [| EVal (AInt    $(TH.varE $ liftName x)) |]
  EVar x TChar   -> Just [| EVal (AChar   $(TH.varE $ liftName x)) |]
  EVar x TString -> Just [| EVal (AString $(TH.varE $ liftName x)) |]  
  _ -> Nothing

exprToPatQ :: AExpr -> PatQ
exprToPatQ = dataToPatQ (const Nothing `extQ` metaPat)

metaPat :: AExpr -> Maybe PatQ
metaPat = \case
  EVar x _ | x `elem` genericMetaVars -> Just $ metaVar x
  EVar x TUnit   -> Just [p| EVal (AUnit   $(metaVar x)) |]
  EVar x TBool   -> Just [p| EVal (ABool   $(metaVar x)) |]
  EVar x TInt    -> Just [p| EVal (AInt    $(metaVar x)) |]
  EVar x TChar   -> Just [p| EVal ($(TH.viewP [|matchChar|] [p|Just $(metaVar x)|])) |]
  EVar x TString -> Just [p| EVal (AString $(metaVar x)) |]
  EFun f es -> Just [p| EFun $(namePat f) $(listP (map exprToPatQ es) ) |]
  _ -> Nothing

matchChar :: AValue -> Maybe AChar
matchChar (AChar c)   = Just c
matchChar (AString s) = AString.toChar s
matchChar _           = Nothing

-- | Lift a Panini 'Name' into a Template Haskell 'TH.Name'
liftName :: Name -> TH.Name
liftName (Name x _)  = mkName $ Text.unpack x

-- | Create a meta variable, i.e., a Haskell variable bound in a pattern. If
-- there is already a variable of the same name in scope, either from outside or
-- inside the pattern, then a view pattern checking for equality is produced.
--
-- Here are some examples, using the 'ρ' quasiquoter defined above:
--
-- > f x [ρ| y = z |] = ...   -->   f x (y :=: z)
-- > f x [p| y = x |] = ...   -->   f x (y :=: ((== x) -> True))
-- > f x [p| y = y |] = ...   -->   f x (y :=: ((== y) -> True))
--
metaVar :: Name -> PatQ
metaVar x = do
  Just knownVars <- TH.getQ
  if x `elem` knownVars
    then do
      TH.viewP [|(== $(TH.varE (liftName x)))|] [p|True|]
    else do
      TH.putQ $ x : knownVars
      TH.lookupValueName (showPretty x) >>= \case
        Nothing -> varP (liftName x)
        Just _  -> TH.viewP [|(== $(TH.varE (liftName x)))|] [p|True|]

namePat :: Name -> PatQ
namePat (Name f _) = [p| Name $(litP (TH.StringL $ Text.unpack f)) _ |]

genericMetaVars :: [Name]
genericMetaVars = mkVars ["e","u","v","w","x","y","z","_"]

implicitTypes :: Set (Name,Base)
implicitTypes = Set.fromList [ (x,t) | (t,xs) <- defs, x <- xs]
 where 
  defs = 
    [ (TUnit    , genericMetaVars)  -- unit type just for parsing
    , (TUnit    , ["_1"])
    , (TBool    , mkVars ["p","q"])
    , (TInt     , mkVars ["i","j","k","l","m","n"])
    , (TChar    , mkVars ["a","b","c"])
    , (TString  , mkVars ["s","t"])
    ]

mkVars :: [String] -> [Name]
mkVars xs = map fromString $ concat [ x : [x ++ y | y <- marks] | x <- xs ]
 where
  marks = [ "'"
          , "\x0302" -- U+0302 ◌̂ COMBINING CIRCUMFLEX ACCENT
          , "\x0304" -- U+0305 ◌̄ COMBINING MACRON
          , "\x0305" -- U+0305 ◌̅ COMBINING OVERLINE
          , "\x0332" -- U+0332 ◌̲ COMBINING LOW LINE
          ]

extQ :: (Typeable a, Typeable b) => (a -> r) -> (b -> r) -> a -> r
extQ f g a = maybe (f a) g (cast a)
