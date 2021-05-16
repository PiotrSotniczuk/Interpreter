-- Haskell module generated by the BNF converter

{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module SkelFlatte where

import Prelude (($), Either(..), String, (++), Show, show)
import qualified AbsFlatte

type Err = Either String
type Result = Err String

failure :: Show a => a -> Result
failure x = Left $ "Undefined case: " ++ show x

transIdent :: AbsFlatte.Ident -> Result
transIdent x = case x of
  AbsFlatte.Ident string -> failure x

transProgram :: AbsFlatte.Program -> Result
transProgram x = case x of
  AbsFlatte.ProgramDef dec -> failure x

transDec :: AbsFlatte.Dec -> Result
transDec x = case x of
  AbsFlatte.FDec type_ ident args block -> failure x
  AbsFlatte.VDec type_ ident -> failure x
  AbsFlatte.VdecInit type_ ident expr -> failure x

transArg :: AbsFlatte.Arg -> Result
transArg x = case x of
  AbsFlatte.ValArg ident type_ -> failure x
  AbsFlatte.RefArg ident type_ -> failure x

transBlock :: AbsFlatte.Block -> Result
transBlock x = case x of
  AbsFlatte.BlockStmt stmts -> failure x

transStmt :: AbsFlatte.Stmt -> Result
transStmt x = case x of
  AbsFlatte.DecStmt dec -> failure x
  AbsFlatte.Assign ident expr -> failure x
  AbsFlatte.Incr ident -> failure x
  AbsFlatte.Decr ident -> failure x
  AbsFlatte.Ret expr -> failure x
  AbsFlatte.If expr block -> failure x
  AbsFlatte.IfElse expr block1 block2 -> failure x
  AbsFlatte.While expr block -> failure x
  AbsFlatte.Break -> failure x
  AbsFlatte.Cont -> failure x
  AbsFlatte.SExp expr -> failure x

transType :: AbsFlatte.Type -> Result
transType x = case x of
  AbsFlatte.Int -> failure x
  AbsFlatte.Str -> failure x
  AbsFlatte.Bool -> failure x
  AbsFlatte.TypTuple types -> failure x

transExpr :: AbsFlatte.Expr -> Result
transExpr x = case x of
  AbsFlatte.EVar ident -> failure x
  AbsFlatte.ELitInt integer -> failure x
  AbsFlatte.ELitStr string -> failure x
  AbsFlatte.ETup tuple -> failure x
  AbsFlatte.ETupTak expr integer -> failure x
  AbsFlatte.ELitTrue -> failure x
  AbsFlatte.ELitFalse -> failure x
  AbsFlatte.ELitMaybe -> failure x
  AbsFlatte.ERunFun ident exprs -> failure x
  AbsFlatte.EMinus expr -> failure x
  AbsFlatte.ENot expr -> failure x
  AbsFlatte.EMul expr1 mulop expr2 -> failure x
  AbsFlatte.EAdd expr1 addop expr2 -> failure x
  AbsFlatte.EComp expr1 compop expr2 -> failure x
  AbsFlatte.EAnd expr1 expr2 -> failure x
  AbsFlatte.EOr expr1 expr2 -> failure x

transTuple :: AbsFlatte.Tuple -> Result
transTuple x = case x of
  AbsFlatte.ETuple exprs -> failure x

transAddOp :: AbsFlatte.AddOp -> Result
transAddOp x = case x of
  AbsFlatte.Plus -> failure x
  AbsFlatte.Minus -> failure x

transMulOp :: AbsFlatte.MulOp -> Result
transMulOp x = case x of
  AbsFlatte.Times -> failure x
  AbsFlatte.Div -> failure x
  AbsFlatte.Mod -> failure x

transCompOp :: AbsFlatte.CompOp -> Result
transCompOp x = case x of
  AbsFlatte.LTH -> failure x
  AbsFlatte.LE -> failure x
  AbsFlatte.GTH -> failure x
  AbsFlatte.GE -> failure x
  AbsFlatte.EQU -> failure x
  AbsFlatte.NE -> failure x
