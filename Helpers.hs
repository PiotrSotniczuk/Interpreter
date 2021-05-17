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
--TODO REF
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


--evalMaybe :: IO Bool
--evalMaybe = do
--    randomNum <- randomRIO (0,1)
--    return (randomNum == 0)

evalExpr :: Expr -> InterpreterM Value
evalExpr (ELitInt val)            = return (VInt val)
evalExpr (ELitStr str)            = return (VString str)
evalExpr (ELitTrue )                = return (VBool True)
evalExpr (ELitFalse)                = return (VBool False)
evalExpr (ELitMaybe)                = do
    --u <- getCurrentTime
    --millis <- nanosSinceEpoch u
    return (VBool True)
    
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
        Div -> do
            if (val2 == 0) then throwError "ERROR: Can't divide by zero"
            else return (VInt (div val1 val2))
        Mod -> do
            if (val2 == 0) then throwError "ERROR: Can't do modulo by zero"
            else return (VInt (mod val1 val2))
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