{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

-- | Pretty-printer for PrintFlatte.
--   Generated by the BNF converter.

module PrintFlatte where

import Prelude
  ( ($), (.)
  , Bool(..), (==), (<)
  , Int, Integer, Double, (+), (-), (*)
  , String, (++)
  , ShowS, showChar, showString
  , all, dropWhile, elem, foldr, id, map, null, replicate, shows, span
  )
import Data.Char ( Char, isSpace )
import qualified AbsFlatte

-- | The top-level printing method.

printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i = \case
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    [";"]        -> showChar ';'
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : ts@(p:_) | closingOrPunctuation p -> showString t . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i     = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t s =
    case (all isSpace t', null spc, null rest) of
      (True , _   , True ) -> []              -- remove trailing space
      (False, _   , True ) -> t'              -- remove trailing space
      (False, True, False) -> t' ++ ' ' : s   -- add space if none
      _                    -> t' ++ s
    where
      t'          = showString t []
      (spc, rest) = span isSpace s

  closingOrPunctuation :: String -> Bool
  closingOrPunctuation [c] = c `elem` closerOrPunct
  closingOrPunctuation _   = False

  closerOrPunct :: String
  closerOrPunct = ")],;"

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- | The printer class does the job.

class Print a where
  prt :: Int -> a -> Doc
  prtList :: Int -> [a] -> Doc
  prtList i = concatD . map (prt i)

instance {-# OVERLAPPABLE #-} Print a => Print [a] where
  prt = prtList

instance Print Char where
  prt     _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList _ s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q = \case
  s | s == q -> showChar '\\' . showChar s
  '\\' -> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  s -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j < i then parenth else id

instance Print Integer where
  prt _ x = doc (shows x)

instance Print Double where
  prt _ x = doc (shows x)

instance Print AbsFlatte.Ident where
  prt _ (AbsFlatte.Ident i) = doc $ showString i

instance Print AbsFlatte.Program where
  prt i = \case
    AbsFlatte.ProgramDef dec -> prPrec i 0 (concatD [prt 0 dec])

instance Print AbsFlatte.Dec where
  prt i = \case
    AbsFlatte.FDec type_ id_ args block -> prPrec i 0 (concatD [prt 0 type_, prt 0 id_, doc (showString "("), prt 0 args, doc (showString ")"), doc (showString ":="), prt 0 block])
    AbsFlatte.VDec type_ id_ -> prPrec i 0 (concatD [prt 0 type_, prt 0 id_])
    AbsFlatte.VdecInit type_ id_ expr -> prPrec i 0 (concatD [prt 0 type_, prt 0 id_, doc (showString ":="), prt 0 expr])

instance Print AbsFlatte.Arg where
  prt i = \case
    AbsFlatte.ValArg id_ type_ -> prPrec i 0 (concatD [prt 0 id_, doc (showString ":"), prt 0 type_])
    AbsFlatte.RefArg id_ type_ -> prPrec i 0 (concatD [prt 0 id_, doc (showString ":&"), prt 0 type_])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print [AbsFlatte.Arg] where
  prt = prtList

instance Print AbsFlatte.Block where
  prt i = \case
    AbsFlatte.BlockStmt stmts -> prPrec i 0 (concatD [doc (showString "{"), prt 0 stmts, doc (showString "}")])

instance Print [AbsFlatte.Stmt] where
  prt = prtList

instance Print AbsFlatte.Stmt where
  prt i = \case
    AbsFlatte.DecStmt dec -> prPrec i 0 (concatD [prt 0 dec, doc (showString ";")])
    AbsFlatte.Assign id_ expr -> prPrec i 0 (concatD [prt 0 id_, doc (showString ":="), prt 0 expr, doc (showString ";")])
    AbsFlatte.Incr id_ -> prPrec i 0 (concatD [prt 0 id_, doc (showString "++"), doc (showString ";")])
    AbsFlatte.Decr id_ -> prPrec i 0 (concatD [prt 0 id_, doc (showString "--"), doc (showString ";")])
    AbsFlatte.Ret expr -> prPrec i 0 (concatD [doc (showString "return"), prt 0 expr, doc (showString ";")])
    AbsFlatte.If expr block -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 block])
    AbsFlatte.IfElse expr block1 block2 -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 block1, doc (showString "else"), prt 0 block2])
    AbsFlatte.While expr block -> prPrec i 0 (concatD [doc (showString "while"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 block])
    AbsFlatte.Break -> prPrec i 0 (concatD [doc (showString "break"), doc (showString ";")])
    AbsFlatte.Cont -> prPrec i 0 (concatD [doc (showString "continue"), doc (showString ";")])
    AbsFlatte.SExp expr -> prPrec i 0 (concatD [prt 0 expr, doc (showString ";")])
  prtList _ [] = concatD []
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print AbsFlatte.Type where
  prt i = \case
    AbsFlatte.Int -> prPrec i 0 (concatD [doc (showString "int")])
    AbsFlatte.Str -> prPrec i 0 (concatD [doc (showString "str")])
    AbsFlatte.Bool -> prPrec i 0 (concatD [doc (showString "bool")])
    AbsFlatte.TypTuple types -> prPrec i 0 (concatD [doc (showString "["), prt 0 types, doc (showString "]")])
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print [AbsFlatte.Type] where
  prt = prtList

instance Print AbsFlatte.Expr where
  prt i = \case
    AbsFlatte.EVar id_ -> prPrec i 6 (concatD [prt 0 id_])
    AbsFlatte.ELitInt n -> prPrec i 6 (concatD [prt 0 n])
    AbsFlatte.ELitStr str -> prPrec i 6 (concatD [prt 0 str])
    AbsFlatte.ETup tuple -> prPrec i 6 (concatD [prt 0 tuple])
    AbsFlatte.ETupTak expr n -> prPrec i 6 (concatD [prt 6 expr, doc (showString "^"), prt 0 n])
    AbsFlatte.ELitTrue -> prPrec i 6 (concatD [doc (showString "true")])
    AbsFlatte.ELitFalse -> prPrec i 6 (concatD [doc (showString "false")])
    AbsFlatte.ELitMaybe -> prPrec i 6 (concatD [doc (showString "maybe")])
    AbsFlatte.ERunFun id_ exprs -> prPrec i 6 (concatD [prt 0 id_, doc (showString "("), prt 0 exprs, doc (showString ")")])
    AbsFlatte.EMinus expr -> prPrec i 5 (concatD [doc (showString "-"), prt 6 expr])
    AbsFlatte.ENot expr -> prPrec i 5 (concatD [doc (showString "!"), prt 6 expr])
    AbsFlatte.EMul expr1 mulop expr2 -> prPrec i 4 (concatD [prt 4 expr1, prt 0 mulop, prt 5 expr2])
    AbsFlatte.EAdd expr1 addop expr2 -> prPrec i 3 (concatD [prt 3 expr1, prt 0 addop, prt 4 expr2])
    AbsFlatte.EComp expr1 compop expr2 -> prPrec i 2 (concatD [prt 2 expr1, prt 0 compop, prt 3 expr2])
    AbsFlatte.EAnd expr1 expr2 -> prPrec i 1 (concatD [prt 2 expr1, doc (showString "&&"), prt 1 expr2])
    AbsFlatte.EOr expr1 expr2 -> prPrec i 0 (concatD [prt 1 expr1, doc (showString "||"), prt 0 expr2])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print AbsFlatte.Tuple where
  prt i = \case
    AbsFlatte.ETuple exprs -> prPrec i 0 (concatD [doc (showString "["), prt 0 exprs, doc (showString "]")])

instance Print [AbsFlatte.Expr] where
  prt = prtList

instance Print AbsFlatte.AddOp where
  prt i = \case
    AbsFlatte.Plus -> prPrec i 0 (concatD [doc (showString "+")])
    AbsFlatte.Minus -> prPrec i 0 (concatD [doc (showString "-")])

instance Print AbsFlatte.MulOp where
  prt i = \case
    AbsFlatte.Times -> prPrec i 0 (concatD [doc (showString "*")])
    AbsFlatte.Div -> prPrec i 0 (concatD [doc (showString "/")])
    AbsFlatte.Mod -> prPrec i 0 (concatD [doc (showString "%")])

instance Print AbsFlatte.CompOp where
  prt i = \case
    AbsFlatte.LTH -> prPrec i 0 (concatD [doc (showString "<")])
    AbsFlatte.LE -> prPrec i 0 (concatD [doc (showString "<=")])
    AbsFlatte.GTH -> prPrec i 0 (concatD [doc (showString ">")])
    AbsFlatte.GE -> prPrec i 0 (concatD [doc (showString ">=")])
    AbsFlatte.EQU -> prPrec i 0 (concatD [doc (showString "==")])
    AbsFlatte.NE -> prPrec i 0 (concatD [doc (showString "!=")])

