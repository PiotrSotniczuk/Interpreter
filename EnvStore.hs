module EnvStore where

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

getTupInit :: [Type] -> InterpreterM [Value]
getTupInit [] = return []
getTupInit (t:ts) = (:) <$> initVal t <*> getTupInit ts

initVal :: Type -> InterpreterM Value
initVal Int = return (VInt 0)
initVal Str = return (VString "")
initVal Bool = return (VBool True)
initVal (TypTuple types) = do
    ts <- getTupInit types
    return (VTup ts)

setValToLoc :: Loc -> Value -> InterpreterM ()
setValToLoc loc val = do
    new_store <- M.insert loc val <$> get
    put new_store

saveVarInEnv :: Type -> Ident -> InterpreterM Env
saveVarInEnv t id = do
    loc <- getNewLoc <$> get
    env <- asks (M.insert id loc)
    init <- initVal t
    setValToLoc loc init
    return env

saveVarWithRef :: Env -> Type -> Ident -> Ident -> InterpreterM Env
saveVarWithRef fun_env new_t id ref_id = do
    loc <- getLocFromVar ref_id
    val <- getValFromLoc loc
    last_t <- getType val
    run_env <- local (const fun_env) (asks (M.insert id loc))
    if last_t == new_t then return run_env
    else throwError "ERROR: referation argument type does not match variable type"

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
        Just v     -> return v

getVarVal :: Ident -> InterpreterM Value
getVarVal id = do
    loc <- getLocFromVar id
    getValFromLoc loc

setValToVar :: Ident -> Value -> InterpreterM ()
setValToVar id val = do   
    act_type <- getType val
    last_val <- getVarVal id
    last_type <- getType last_val    
    loc <- getLocFromVar id
    if (act_type == last_type) then
        setValToLoc loc val
    else 
        throwError "TYPE ERROR: value does not match declared type"

getTupTypes :: [Value] -> InterpreterM [Type]
getTupTypes [] = return []
getTupTypes (x:xs) = (:) <$> getType x <*> getTupTypes xs

getType :: Value -> InterpreterM Type
getType (VBool a) = return Bool
getType (VString a) = return Str
getType (VInt a) = return Int
getType (VFunc env t args block) = return t
getType (VTup []) = throwError "ERROR: don't use empty tuples"
getType (VTup vs) = do
    types <- getTupTypes vs
    return (TypTuple types)
