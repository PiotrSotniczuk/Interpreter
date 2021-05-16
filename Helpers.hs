module Helpers where

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.IO.Class

import qualified Data.Map.Lazy as M

import AbsFlatte
import LexFlatte
import ParFlatte
import SkelFlatte
import ErrM
import PrintFlatte

import DataTypes
import EnvStore

declare :: Dec -> InterpreterM Env
declare (FDec t id args block) = do
    env <- saveVarInEnv t id
    local (const env) (setValToVar id (VFunc env args block))
    return env
declare (VDec t id) = do
    env <- saveVarInEnv t id
    return env
declare (VdecInit t id expr) = do
    val <- evalExpr expr
    env <- saveVarInEnv t id
    local (const env) (setValToVar id val)
    return env

evalFuncExprs :: [Expr] -> InterpreterM [Value]
evalFuncExprs [] = return []
evalFuncExprs (x:xs) = (:) <$> evalExpr x <*> evalFuncExprs xs

assignFuncArgs :: [Arg] -> [Value] -> InterpreterM Env
--TODO check length
--TODO REF
assignFuncArgs [] [] = ask
assignFuncArgs ((ValArg id t):args) (v:vs) = do
    env <- saveVarInEnv t id
    local (const env) $ setValToVar id v
    local (const env) $ assignFuncArgs args vs


executeStmt :: Stmt -> InterpreterM RetInfo
executeStmt (Ret expr) = do
    val <- evalExpr expr
    return (Return val)
executeStmt (DecStmt dec) = do
    env <- declare dec
    return (RetEnv env)
executeStmt (Assign id expr) = do
    val <- evalExpr expr
    env <- ask
    setValToVar id val
    return (RetEnv env)
executeStmt (Incr id) = do
    VInt val <- getVarVal id
    env <- ask
    setValToVar id (VInt (val + 1))
    return (RetEnv env)
executeStmt (Decr id) = do
    VInt val <- getVarVal id
    env <- ask
    setValToVar id (VInt (val - 1))
    return (RetEnv env)
executeStmt (If expr block) = do
    VBool bool <- evalExpr expr
    env <- ask
    if bool then executeBlock block
    else return (RetEnv env)
executeStmt (IfElse expr blockT blockF) = do
    VBool bool <- evalExpr expr
    env <- ask
    if bool then executeBlock blockT
    else executeBlock blockF
executeStmt (SExp expr) = do
    val <- evalExpr expr
    env <- ask
    return (RetEnv env)
executeStmt (Break) = return RetBreak
executeStmt (continue) = return RetContinue


executeBlock :: Block -> InterpreterM RetInfo
executeBlock (BlockStmt []) = do
    env <- ask
    return (RetEnv env)
executeBlock (BlockStmt (stmt:stmts)) = do
    ret <- executeStmt stmt
    case ret of
        Return val      -> return (Return val)
        RetEnv env   -> local (const env) $ executeBlock (BlockStmt stmts)


--evalMaybe :: IO Bool
--evalMaybe = do
--    randomNum <- randomRIO (0,1)
--    return (randomNum == 0)

evalExpr :: Expr -> InterpreterM Value
evalExpr (ELitInt val)            = return (VInt val)
evalExpr (ELitStr str)            = return (VString str)
evalExpr (ELitTrue )                = return (VBool True)
evalExpr (ELitFalse)                = return (VBool False)
--evalExpr (ELitMaybe)                = return (VBool True)
evalExpr (EVar id)             = getVarVal id
evalExpr (EMinus expr) = do
    VInt int <- evalExpr expr
    return (VInt (-1*int))
evalExpr (ENot expr) = do
    VBool bool <- evalExpr expr
    return (VBool (not bool))
evalExpr (EMul expr1 mulOp expr2) = do
    VInt val1 <- evalExpr expr1
    VInt val2 <- evalExpr expr2
    case mulOp of
        Times -> return (VInt (val1 * val2))
        Div -> return (VInt (div val1 val2))
        Mod -> return (VInt (mod val1 val2))
evalExpr (EAdd expr1 addOp expr2) = do
    VInt val1 <- evalExpr expr1
    VInt val2 <- evalExpr expr2
    case addOp of
        Plus -> return (VInt (val1 + val2))
        Minus -> return (VInt  (val1 - val2))
evalExpr (EComp expr1 compOp expr2) = do
    VInt val1 <- evalExpr expr1
    VInt val2 <- evalExpr expr2
    case compOp of
        LTH -> return (VBool (val1 < val2))
        LE -> return (VBool (val1 <= val2)) 
        GTH -> return (VBool (val1 > val2)) 
        GE -> return (VBool (val1 >= val2)) 
        EQU -> return (VBool (val1 == val2)) 
        NE -> return (VBool (val1 /= val2))
evalExpr (ERunFun (Ident "print") in_args) = do
    in_vals <- evalFuncExprs in_args
    liftIO (putStrLn $ "PRINT: " ++ (show in_vals))
    return (VInt 0)
evalExpr (ERunFun id in_args)       = do
    VFunc env env_args block <- evalExpr (EVar id)
    in_vals                   <- evalFuncExprs in_args
    env'                 <- local (const env)  $ assignFuncArgs env_args in_vals
    ret                  <- local (const env') $ executeBlock block
    case ret of
        Return val      -> return val
        --breakOrCont     -> throwError "Error: break/continue out of loop"


runInterpreter :: Program -> InterpreterM Integer
runInterpreter (ProgramDef mainDec) = do
    env      <- declare mainDec
    VInt val <- local (const env) (evalExpr (ERunFun (Ident "main") []))
    return val