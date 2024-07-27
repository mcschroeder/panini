{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE RecordWildCards #-}

{-|
This module defines pretty printing for Python ASTs with type annotations. Note
that these pretty printing instances are for debug purposes only---they do not
produce valid Python source code!
-}
module Panini.Frontend.Python.Typing.Pretty where

import Data.List qualified as List
import Panini.Frontend.Python.AST as Py
import Panini.Frontend.Python.Pretty ()
import Panini.Frontend.Python.Typing.PyType (PyType)
import Panini.Frontend.Python.Typing.PyType qualified as PyType
import Panini.Frontend.Python.Typing.TypeInfo
import Panini.Pretty
import Prelude
import Prettyprinter qualified as PP

------------------------------------------------------------------------------

instance Pretty PyType where
  pretty = \case
    PyType.Any_ Nothing      -> "Any"
    PyType.Any_ (Just i)     -> ann Highlight ("μ" <> pretty i)
    PyType.TypeVar s         -> pretty s
    PyType.Union_ ts         -> "Union" <> params ts
    PyType.Callable xs y     -> "Callable[" <> params xs <> "," <> pretty y <> "]"
    PyType.None              -> "None"
    PyType.Bool              -> "bool"
    PyType.Int               -> "int"
    PyType.Float             -> "float"
    PyType.Complex           -> "complex"
    PyType.Str               -> "str"
    PyType.Bytes             -> "bytes"
    PyType.Bytearray         -> "bytearray"
    PyType.Object            -> "object"
    PyType.List t            -> "list" <> param t
    PyType.Dict k v          -> "dict" <> params [k,v]
    PyType.Tuple ts          -> "tuple" <> params ts
    PyType.Iterable t        -> "Iterable" <> param t
    PyType.Sequence t        -> "Sequence" <> param t
    PyType.Collection t      -> "Collection" <> param t
    PyType.Mapping k v       -> "Mapping" <> params [k,v]
    PyType.Number            -> "Number"
    PyType.Buffer            -> "Buffer"
    PyType.AnyStr            -> "AnyStr"
    PyType.Hashable          -> "Hashable"
    PyType.SupportsAbs t     -> "SupportsAbs" <> param t
    PyType.SupportsRound t   -> "SupportsRound" <> param t
    PyType.SupportsIndex     -> "SupportsIndex"
    PyType.SupportsComplex   -> "SupportsComplex"
    PyType.SupportsFloat     -> "SupportsFloat"
    PyType.AsyncIterable t   -> "AsyncIterable" <> param t
    PyType.AsyncIterator t   -> "AsyncIterator" <> param t
    PyType.Slice             -> "Slice"
    PyType.Ellipsis          -> "Ellipsis"
    PyType.Set t             -> "Set" <> param t
    PyType.ConvertibleToInt  -> "ConvertibleToInt"
    PyType.ConvertibleToFloat -> "ConvertibleToFloat"
    PyType.ReadableBuffer    -> "ReadableBuffer"
    PyType.Iterator t        -> "Iterator" <> param t
    PyType.SupportsDivMod t1 t2 -> "SupportsDivMod" <> params [t1,t2]
    PyType.SupportsRDivMod t1 t2 -> "SupportsRDivMod" <> params [t1,t2]
    PyType.NoReturn          -> "NoReturn"
    PyType.SupportsIter t    -> "SupportsIter" <> param t
    PyType.SupportsNext t    -> "SupportsNext" <> param t
    PyType.Sized             -> "Sized"
   where
    param  t  = "[" <> pretty t <> "]"
    params ts = "[" <> mconcat (List.intersperse "," (map pretty ts)) <> "]"

------------------------------------------------------------------------------

withT :: (Maybe PyType, a) -> Doc -> Doc
withT (Nothing, _) d = d
withT (Just ty, _) d = d <> ann Margin (":" <> pretty ty)

parensWithT :: (Maybe PyType, a) -> Doc -> Doc
parensWithT (Nothing, _) d = d
parensWithT (Just ty, _) d = (parens d) <> ann Margin (":" <> pretty ty)

prettyWithT :: (Pretty (t a), Functor t, Annotated t) => Typed t a -> Doc
prettyWithT e = case fst (annot e) of
  Nothing -> pretty (eraseTypes e)
  Just ty -> pretty (eraseTypes e) <> ann Margin (":" <> pretty ty)

eraseTypes :: Functor t => Typed t a -> t a
eraseTypes = fmap snd

------------------------------------------------------------------------------

-- these instance are for debugging only; they do not produce valid Python!

instance {-# OVERLAPPING #-} Pretty (Typed Module a) where
  pretty (Module stmts) = vcat $ map pretty stmts

instance {-# OVERLAPPING #-} Pretty (Typed Statement a) where
  pretty = \case   
    e@Import     {} -> pretty (eraseTypes e)
    e@FromImport {} -> pretty (eraseTypes e)
    
    While {..} -> 
      "while" <+> pretty while_cond <> ":" 
      <\\> prettySuite while_body 
      <\\> prettySuiteOpt "else" while_else
    
    For {..} ->
      "for" <+> commaList for_targets <+> "in" <+> pretty for_generator <> ":" 
      <\\> prettySuite for_body 
      <\\> prettySuiteOpt "else" for_else

    AsyncFor{..} -> "async " <> pretty for_stmt

    Fun {..} -> 
      "def" <+> prettyWithT fun_name <> parens (commaList fun_args) 
      <> opt ((" -> " <>) . pretty) fun_result_annotation <> ":" 
      <\\> prettySuite fun_body
    
    AsyncFun {..} -> "async " <+> pretty fun_def
    
    Class {..} -> 
      "class" <+> pretty class_name <> prettyOptionalList class_args <> ":" 
      <\\> prettySuite class_body
    
    Conditional {..} -> 
      prettyGuards "if" cond_guards 
      <\\> prettySuiteOpt "else" cond_else
     where
      prettyGuards _ [] = mempty
      prettyGuards kw ((cond,body):xs) = 
        kw <+> pretty cond <> ":" <\\> 
        prettySuite body <\\> 
        prettyGuards "elif" xs
    
    Assign {..} -> equalsList assign_to <+> PP.equals <+> pretty assign_expr
    
    AugmentedAssign {..} -> 
      pretty aug_assign_to <+> pretty aug_assign_op <+> pretty aug_assign_expr

    AnnotatedAssign {..} -> 
      pretty ann_assign_to <+> ":" <+> pretty ann_assign_annotation 
      <+> opt (("=" <+>) . pretty) ann_assign_expr

    Decorated {..} -> 
      vcat (map pretty decorated_decorators) 
      <\\> pretty decorated_def
    
    Return {..} -> "return" <+> pretty return_expr
    
    Try {..} -> 
      "try:" 
      <\\> prettySuite try_body 
      <\\> prettyHandlers try_excepts 
      <\\> prettySuiteOpt "else" try_else 
      <\\> prettySuiteOpt "finally" try_finally
     where
      prettyHandlers = foldr (\next r -> pretty next <\\> r) mempty
    
    Raise {..} -> "raise" <+> pretty raise_expr  
    
    With {..} -> 
      "with" <+> prettyWithContext with_context <+> ":" 
      <\\> prettySuite with_body
     where
      prettyWithContext = PP.hcat . PP.punctuate "," . map prettyAs
      prettyAs (e, Nothing) = pretty e
      prettyAs (e, Just as) = pretty e <+> "as" <+> pretty as
    
    AsyncWith {..} -> "async "  <+> pretty with_stmt
    Pass      {}   -> "pass"
    Break     {}   -> "break"
    Continue  {}   -> "continue"
    Delete    {..} -> "del"      <+> commaList del_exprs
    StmtExpr  {..} -> pretty stmt_expr
    Global    {..} -> "global"   <+> commaList global_vars
    NonLocal  {..} -> "nonlocal" <+> commaList nonLocal_vars
    Assert    {..} -> "assert"   <+> commaList assert_exprs

    Print {..} -> 
      "print" <> (if print_chevron then " >>" else mempty) 
      <+> PP.hcat (PP.punctuate "," (map pretty print_exprs)) 
      <> if print_trailing_comma then "," else mempty
    
    Exec {..} ->
      "exec" <+> pretty exec_expr 
      <+> opt (\(globals, next) -> "in" <+> pretty globals 
        <+> opt (("," <+>) . pretty) next) exec_globals_locals

instance {-# OVERLAPPING #-} Pretty (Typed Expr a) where
  pretty = \case
    e@Var             {} -> prettyWithT e
    e@Int             {} -> prettyWithT e
    e@LongInt         {} -> prettyWithT e
    e@Float           {} -> prettyWithT e
    e@Imaginary       {} -> prettyWithT e
    e@Bool            {} -> prettyWithT e
    e@None            {} -> prettyWithT e
    e@Ellipsis        {} -> prettyWithT e
    e@ByteStrings     {} -> prettyWithT e
    e@Strings         {} -> prettyWithT e
    e@UnicodeStrings  {} -> prettyWithT e    
    
    Call {..} -> pretty call_fun <> parens (commaList call_args)
    
    Subscript {..} -> withT expr_annot $ 
      pretty subscriptee <> brackets (pretty subscript_expr)
    
    SlicedExpr {..} -> withT expr_annot $
      pretty slicee <> brackets (commaList slices)
    
    CondExpr {..} -> parensWithT expr_annot $
      pretty ce_true_branch 
      <+> "if" <+> pretty ce_condition 
      <+> "else" <+> pretty ce_false_branch
      
    BinaryOp {..} -> parensWithT expr_annot $
      pretty left_op_arg <+> pretty operator <+> pretty right_op_arg

    UnaryOp {..} -> pretty operator <+> pretty op_arg  

    Dot {..} -> parensWithT expr_annot $ 
      pretty dot_expr <> "." <> pretty dot_attribute

    Lambda {..} -> parensWithT expr_annot $
      "lambda" <+> commaList lambda_args <> ":" <+> pretty lambda_body

    Tuple {..} -> case tuple_exprs of
      []  -> withT expr_annot "()"
      [e] -> parensWithT expr_annot $ pretty e <> ","
      _   -> parensWithT expr_annot $ commaList tuple_exprs

    Yield {..} -> "yield" <+> pretty yield_arg
    Await {..} -> "await" <+> pretty await_expr

    Generator  {..} -> withT expr_annot $ parens   $ pretty    gen_comprehension
    ListComp   {..} -> withT expr_annot $ brackets $ pretty    list_comprehension
    List       {..} -> withT expr_annot $ brackets $ commaList list_exprs
    Dictionary {..} -> withT expr_annot $ braces   $ commaList dict_mappings
    DictComp   {..} -> withT expr_annot $ braces   $ pretty    dict_comprehension
    Py.Set     {..} -> withT expr_annot $ braces   $ commaList set_exprs
    SetComp    {..} -> withT expr_annot $ braces   $ pretty    set_comprehension
    
    Paren {..} -> parens $ pretty paren_expr    
    StringConversion {..} -> withT expr_anot $ "`" <> pretty backquoted_expr <> "`"
    Starred {..} -> parensWithT expr_annot $ "*" <> pretty starred_expr

instance {-# OVERLAPPING #-} Pretty (Typed Comprehension a) where
  pretty Comprehension {..} = 
    pretty comprehension_expr <+> pretty comprehension_for

instance {-# OVERLAPPING #-} Pretty (Typed ComprehensionExpr a) where
  pretty = \case
    ComprehensionExpr e -> pretty e
    ComprehensionDict d -> pretty d

instance {-# OVERLAPPING #-} Pretty (Typed CompFor a) where
  pretty CompFor {..} =
    (if comp_for_async then "async for" else "for") <+> commaList comp_for_exprs
    <+> "in" <+> pretty comp_in_expr <+> pretty comp_for_iter

instance {-# OVERLAPPING #-} Pretty (Typed CompIf a) where
  pretty CompIf {..} = "if" <+> pretty comp_if <+> pretty comp_if_iter

instance {-# OVERLAPPING #-} Pretty (Typed CompIter a) where
  pretty = \case
    IterFor {..} -> pretty comp_iter_for
    IterIf  {..} -> pretty comp_iter_if

instance {-# OVERLAPPING #-} Pretty (Typed YieldArg a) where
   pretty = \case
    YieldFrom e _ -> "from" <+> pretty e
    YieldExpr e   ->            pretty e

instance {-# OVERLAPPING #-} Pretty (Typed DictKeyDatumList a) where
   pretty = \case
    DictMappingPair key val -> pretty key <> ":" <+> pretty val
    DictUnpacking expr      -> "**" <> pretty expr

instance {-# OVERLAPPING #-} Pretty (Typed Slice a) where
   pretty = \case
    SliceProper   {..} -> 
      pretty slice_lower 
      <> ":" <> pretty slice_upper 
      <> opt ((":" <>) . pretty) slice_stride
    
    SliceExpr {..} -> pretty slice_expr
    SliceEllipsis {} -> "..."

instance {-# OVERLAPPING #-} Pretty (Typed Parameter a) where
  pretty = \case
    Param {..} -> 
      withT param_annot (pretty param_name)
      <> opt ((":" <>) . pretty) param_py_annotation
      <> opt (("=" <>) . pretty) param_default

    VarArgsPos {..} -> 
      withT param_annot ("*" <> pretty param_name) 
      <> opt ((":" <>) . pretty) param_py_annotation

    VarArgsKeyword {..} -> 
      withT param_annot ("**" <> pretty param_name)
      <> opt ((":" <>) . pretty) param_py_annotation

    EndPositional {..} -> withT param_annot ("*") 

    UnPackTuple {..} -> 
      withT param_annot (pretty (eraseTypes param_unpack_tuple)) 
      <> opt (("=" <>) . pretty) param_default

instance {-# OVERLAPPING #-} Pretty (Typed Op a) where
  pretty = prettyWithT

instance {-# OVERLAPPING #-} Pretty (Typed AssignOp a) where
  pretty = prettyWithT

instance {-# OVERLAPPING #-} Pretty (Typed Argument a) where
  pretty = \case
    ArgExpr           {..} -> pretty arg_expr
    ArgVarArgsPos     {..} -> "*" <> pretty arg_expr
    ArgVarArgsKeyword {..} -> "**" <> pretty arg_expr
    ArgKeyword        {..} -> pretty arg_keyword <> "=" <> pretty arg_expr

instance {-# OVERLAPPING #-} Pretty (Typed Decorator a) where
  pretty Decorator {..} =
    "@" <> prettyDottedName decorator_name <+> prettyOptionalList decorator_args

prettyDottedName :: DottedName a -> Doc
prettyDottedName = \case
  []           -> mempty
  [x]          -> pretty x
  (x:xs@(_:_)) -> pretty x <> dot <> prettyDottedName xs

instance {-# OVERLAPPING #-} Pretty (Typed Handler a) where
  pretty Handler {..} = 
    pretty handler_clause <> ":" <\\> prettySuite handler_suite

instance  {-# OVERLAPPING #-} Pretty (Typed RaiseExpr a) where
  pretty = \case
    RaiseV3 e -> 
      opt (\(e1, fromE) -> pretty e1 <+> 
      opt (("from" <+>) . pretty) fromE) e
    RaiseV2 e ->
      opt (\(e1, next1) -> pretty e1 <>
      opt (\(e2, next2) -> "," <+> pretty e2 <>
      opt (\e3 -> "," <+> pretty e3) next2) next1) e

commaList :: Pretty a => [a] -> Doc
commaList = PP.hsep . PP.punctuate "," . map pretty

prettySuite :: [Typed Statement a] -> Doc
prettySuite = PP.indent 4 . vcat . map pretty

prettySuiteOpt :: String -> [Typed Statement a] -> Doc
prettySuiteOpt kw = \case
  [] -> mempty
  xs -> pretty kw <> ":" <\\> prettySuite xs
  
prettyOptionalList :: Pretty a => [a] -> Doc
prettyOptionalList = \case
  [] -> mempty
  xs -> parens $ commaList xs

equalsList :: Pretty a => [a] -> Doc
equalsList = PP.hsep . PP.punctuate (PP.space <> PP.equals) . map pretty

opt :: (a -> Doc) -> Maybe a -> Doc
opt f = maybe mempty f
