module StaticChecker where

import AbsInstant
import Data.Set
import Control.Monad.Except
import Control.Monad.State
import System.IO (stderr, hPutStrLn)

type Env = Set Ident
type Res = StateT Env (ExceptT String IO)

staticCheck :: Program -> IO Bool
staticCheck program = do
	result <- runExceptT (runStateT (checkProgram program) empty)
	case result of
		Right _ -> return True
		Left errorMsg -> do
			hPutStrLn stderr $ "Static check failed: " ++ errorMsg
			return False



checkProgram :: Program -> Res ()
checkProgram (Prog stmts) = do
	mapM_ checkStmt stmts
	return ()
checkProgram _ = error "Unknown program structure."

checkStmt :: Stmt -> Res ()
checkStmt _ = return ()

