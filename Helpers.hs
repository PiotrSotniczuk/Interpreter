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

import System.Random

declare :: Dec -> InterpreterM Env
declare (FDec t id args block) = do
    env <- saveVarInEnv t id
    local (const env) (setValToVar id (VFunc env t args block))
    return env
declare (VDec t id) = do
    env <- saveVarInEnv t id
    return env
declare (VdecInit t id expr) = do
    val <- evalExpr expr
    env <- saveVarInEnv t id
    local (const env) (setValToVar id val)
    return env

evalExprs :: [Expr] -> InterpreterM [Value]
evalExprs [] = return []
evalExprs (x:xs) = (:) <$> evalExpr x <*> evalExprs xs

assignFuncArgs :: Env -> [Arg] -> [Expr] -> InterpreterM Env
assignFuncArgs fun_env [] [] = return fun_env
assignFuncArgs fun_env [] exprs = throwError "ERROR: number of args for function not correct" 
assignFuncArgs fun_env args [] = throwError "ERROR: number of args for function not correct" 
assignFuncArgs fun_env ((ValArg id t):args) (expr:exprs) = do
    run_env <- local (const fun_env) (saveVarInEnv t id)
    val <- evalExpr expr
    local (const run_env) (setValToVar id val)
    assignFuncArgs run_env args exprs
assignFuncArgs fun_env ((RefArg arg_id t):args) ((EVar ref_id):exprs) = do
    run_env <- saveVarWithRef fun_env t arg_id ref_id
    assignFuncArgs run_env args exprs
assignFuncArgs fun_env args exprs = throwError "ERROR: Probably Reference argument does not receive variable" 


exprToBool :: Expr -> InterpreterM Bool
exprToBool expr = do
    val <- evalExpr expr
    case val of
        VBool bool -> if bool then return True
                      else return False
        VInt int -> if int == 0 then return False
                      else return True
        any -> return True

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
    bool <- exprToBool expr
    env <- ask
    if bool then executeBlock block
    else return (RetEnv env)
executeStmt (IfElse expr blockT blockF) = do
    bool <- exprToBool expr
    env <- ask
    if bool then executeBlock blockT
    else executeBlock blockF
executeStmt (SExp expr) = do
    val <- evalExpr expr
    env <- ask
    return (RetEnv env)
executeStmt (Break) = return RetBreak
executeStmt (Cont) = return RetContinue
executeStmt (While expr block) = do
    bool <- exprToBool expr
    env <- ask
    if bool then do
        ret <- executeBlock block
        case ret of
            Return val -> return (Return val)
            RetBreak -> return (RetEnv env)
            any -> executeStmt (While expr block)
    else return (RetEnv env)

executeBlock :: Block -> InterpreterM RetInfo
executeBlock (BlockStmt []) = do
    env <- ask
    return (RetEnv env)
executeBlock (BlockStmt (stmt:stmts)) = do
    ret <- executeStmt stmt
    case ret of
        RetEnv env   -> local (const env) $ executeBlock (BlockStmt stmts)
        any -> return any

evalMulOp :: Value -> Value -> MulOp -> InterpreterM Value
evalMulOp (VInt val1) (VInt val2) mulOp = do
    case mulOp of
        Times -> return (VInt (val1 * val2))
        Div -> do
            if (val2 == 0) then throwError "ERROR: Can't divide by zero"
            else return (VInt (div val1 val2))
        Mod -> do
            if (val2 == 0) then throwError "ERROR: Can't do modulo by zero"
            else return (VInt (mod val1 val2))
evalMulOp a b c = throwError "ERROR: aritmetic operations only apply to Ints"

evalAddOp :: Value -> Value -> AddOp -> InterpreterM Value
evalAddOp (VInt val1) (VInt val2) addOp = do
    case addOp of
        Plus -> return (VInt (val1 + val2))
        Minus -> return (VInt  (val1 - val2))
evalAddOp a b c = throwError "ERROR: aritmetic operations only apply to Ints"

evalCompOp :: Value -> Value -> CompOp -> InterpreterM Value
evalCompOp (VInt val1) (VInt val2) compOp = do
    case compOp of
        LTH -> return (VBool (val1 < val2))
        LE -> return (VBool (val1 <= val2)) 
        GTH -> return (VBool (val1 > val2)) 
        GE -> return (VBool (val1 >= val2)) 
        EQU -> return (VBool (val1 == val2)) 
        NE -> return (VBool (val1 /= val2))
evalCompOp (VBool val1) (VBool val2) compOp = do
    case compOp of
        EQU -> return (VBool (val1 == val2)) 
        NE -> return (VBool (val1 /= val2))
        any -> throwError "ERROR: bool can be compared only through '==' and '!='"
evalCompOp a b c = throwError "ERROR: compare operations only apply to Ints"
    
evalOr :: Value -> Value -> InterpreterM Value
evalOr (VBool b1) (VBool b2) = do
    return (VBool (b1 || b2))
evalOr a b = throwError "ERROR: 'or' operation only apply to Bool"

evalAnd :: Value -> Value -> InterpreterM Value
evalAnd (VBool b1) (VBool b2) = do
    return (VBool (b1 && b2))
evalAnd a b = throwError "ERROR: 'and' operation only apply to Bool"


evalExpr :: Expr -> InterpreterM Value
evalExpr (ELitInt val)            = return (VInt val)
evalExpr (ELitStr str)            = return (VString str)
evalExpr (ELitTrue )                = return (VBool True)
evalExpr (ELitFalse)                = return (VBool False)
evalExpr (ELitMaybe)                = do
    bool <- liftIO (randomIO )
    return (VBool bool)
evalExpr (ETup exprs) = do
    vals <- evalExprs exprs
    return (VTup vals)
evalExpr (ETupTak expr int) = do
    val <- evalExpr expr
    case val of
        VTup vals -> do
            return (vals !! (fromIntegral int))
        any -> throwError "'^' only apply to tuples"
evalExpr (EVar id)             = getVarVal id
evalExpr (EMinus expr) = do
    val <- evalExpr expr
    case val of
        VInt int -> return (VInt (-1*int))
        any -> throwError "ERROR: minus can be applied only to int type"
evalExpr (ENot expr) = do
    val <- evalExpr expr
    case val of
        VBool bool -> return (VBool (not bool))
        any -> throwError "ERROR: not can be applied only to bool type"
evalExpr (EMul expr1 mulOp expr2) = do
    val1 <- evalExpr expr1
    val2 <- evalExpr expr2
    res <- evalMulOp val1 val2 mulOp
    return res
evalExpr (EAdd expr1 addOp expr2) = do
    val1 <- evalExpr expr1
    val2 <- evalExpr expr2
    res <- evalAddOp val1 val2 addOp
    return res
evalExpr (EComp expr1 compOp expr2) = do
    val1 <- evalExpr expr1
    val2 <- evalExpr expr2
    res <- evalCompOp val1 val2 compOp
    return res
evalExpr (EOr expr1 expr2) = do
    val1 <- evalExpr expr1
    val2 <- evalExpr expr2
    res <- evalOr val1 val2
    return res
evalExpr (EAnd expr1 expr2) = do
    val1 <- evalExpr expr1
    val2 <- evalExpr expr2
    res <- evalAnd val1 val2
    return res
evalExpr (ERunFun (Ident "print") exprs) = do
    vals <- evalExprs exprs
    liftIO (putStrLn $ "PRINT: " ++ (show vals))
    return (VInt 0)
evalExpr (ERunFun id exprs)       = do
    VFunc fun_env t args block <- evalExpr (EVar id)
    run_env <- assignFuncArgs fun_env args exprs
    ret  <- local (const run_env) $ executeBlock block
    case ret of
        Return val -> do
            retType <- getType val
            if t == retType then return val
            else throwError "ERROR: function returns mismatched type"                          
        RetEnv env -> throwError "No return statement"
        any -> throwError "Use of continue/break out of while"


runInterpreter :: Program -> InterpreterM Integer
runInterpreter (ProgramDef mainDec) = do
    env      <- declare mainDec
    VInt val <- local (const env) (evalExpr (ERunFun (Ident "main") []))
    return val