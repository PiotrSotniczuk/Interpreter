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

getNewLoc :: Store -> Int
getNewLoc s = M.size s + 1

setValToLoc :: Loc -> Value -> InterpreterM ()
setValToLoc loc val = do
    new_store <- M.insert loc val <$> get
    put new_store

saveVarInEnv :: Type -> Ident -> InterpreterM Env
saveVarInEnv t id = do
    loc <- getNewLoc <$> get
    env <- asks (M.insert id loc)
    setValToLoc loc VNull
    return env

getLocFromVar :: Ident -> InterpreterM Loc
getLocFromVar id = do
    loc <- asks (M.lookup id)
    case loc of
        Nothing   -> throwError $ "Error: variable " ++ show id ++ " is not declared"
        Just addr -> return addr

getValFromLoc :: Loc -> InterpreterM Value
getValFromLoc loc = do
    val <- M.lookup loc <$> get
    case val of
        Nothing    -> throwError "CRITICAL ERROR: This never should've happen"
        Just VNull -> throwError "Error: variable is not assigned"
        Just v     -> return v

getVarVal :: Ident -> InterpreterM Value
getVarVal id = do
    loc <- getLocFromVar id
    getValFromLoc loc

setValToVar :: Ident -> Value -> InterpreterM ()
setValToVar id val = do
    loc <- getLocFromVar id
    setValToLoc loc val


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
--evalExpr (EUnar not expr)      = notVBool    <$> evalExpr expr
--evalExpr (ELog expr1 op expr2) = logVBool op <$> evalExpr expr1 <*> evalExpr expr2
--evalExpr (ECmp expr1 op expr2) = cmpVInt  op <$> evalExpr expr1 <*> evalExpr expr2
--evalExpr (EMul expr1 op expr2) = mulVInt  op <$> evalExpr expr1 <*> evalExpr expr2
--evalExpr (EAdd expr1 op expr2) = addVInt  op <$> evalExpr expr1 <*> evalExpr expr2
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
    liftIO (putStrLn (show mainDec))
    env      <- declare mainDec
    VInt val <- local (const env) (evalExpr (ERunFun (Ident "main") []))
    return val