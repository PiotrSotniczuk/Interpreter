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

type Loc = Int
type Env = M.Map Ident Loc
type Store = M.Map Loc Value

data Value = VInt Integer | VBool Bool | VString String | VFunc Env [Arg] Block | VNull
    deriving (Eq, Ord)

data RetInfo = Return Value | RetEnv Env | Break | Continue

initEnv :: Env
initEnv = M.empty

initStore :: Store
initStore = M.empty


type InterpreterM a = ReaderT Env (ExceptT String (StateT Store IO)) a

runInterM :: Env -> Store -> InterpreterM a -> IO (Either String a, Store)
runInterM env state ev = runStateT (runExceptT (runReaderT ev env)) state

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
declare (FDec pos t id args block) = do
    env <- saveVarInEnv t id
    local (const env) (setValToVar id (VFunc env args block))
    return env

evalFuncExprs :: [Expr] -> InterpreterM [Value]
evalFuncExprs [] = return []
evalFuncExprs (x:xs) = (:) <$> evalExpr x <*> evalFuncExprs xs

assignFuncArgs :: [Arg] -> [Value] -> InterpreterM Env
--TODO check length
--TODO REF
assignFuncArgs [] [] = ask
assignFuncArgs ((ValArg pos id t):args) (v:vs) = do
    env <- saveVarInEnv t id
    local (const env) $ setValToVar id v
    local (const env) $ assignFuncArgs args vs


executeStmt :: Stmt -> InterpreterM RetInfo
executeStmt (Ret pos expr) = do
    val <- evalExpr expr
    return (Return val)

executeBlock :: Block -> InterpreterM RetInfo
executeBlock (Block pos []) = do
    env <- ask
    return (RetEnv env)
executeBlock (Block pos (stmt:stmts)) = do
    ret <- executeStmt stmt
    case ret of
        Return val      -> return (Return val)
        RetEnv env   -> local (const env) $ executeBlock (Block pos stmts)


evalExpr :: Expr -> InterpreterM Value
evalExpr (ELitInt pos val)            = return (VInt val)
evalExpr (ELitStr pos str)            = return (VString str)
--evalExpr ETrue                 = return (VBool True)
--evalExpr EFalse                = return (VBool False)
evalExpr (EVar pos id)             = getVarVal id
--evalExpr (EUnar not expr)      = notVBool    <$> evalExpr expr
--evalExpr (ELog expr1 op expr2) = logVBool op <$> evalExpr expr1 <*> evalExpr expr2
--evalExpr (ECmp expr1 op expr2) = cmpVInt  op <$> evalExpr expr1 <*> evalExpr expr2
--evalExpr (EMul expr1 op expr2) = mulVInt  op <$> evalExpr expr1 <*> evalExpr expr2
--evalExpr (EAdd pos expr1 op expr2) = addVInt  op <$> evalExpr expr1 <*> evalExpr expr2
evalExpr (ERunFun pos id in_args)       = do
    VFunc env env_args block <- evalExpr (EVar pos id)
    in_vals                   <- evalFuncExprs in_args
    env'                 <- local (const env)  $ assignFuncArgs env_args in_vals
    ret                  <- local (const env') $ executeBlock block
    case ret of
        Return val      -> return val
        --breakOrCont     -> throwError "Error: break/continue out of loop"


runInterpreter :: Program -> InterpreterM Integer
runInterpreter (ProgramDef pos mainDec) = do
    liftIO (putStrLn (show mainDec))
    env      <- declare mainDec
    VInt val <- local (const env) (evalExpr (ERunFun pos (Ident "main") []))
    return val