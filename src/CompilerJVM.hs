module CompilerJVM where

import Data.Map
import Control.Monad.Reader
import Control.Monad.Except
-- TODO use except

import AbsInstant
import ErrM
import LexInstant
import ParInstant
import PrintInstant

type Loc = Int
type Env = Map Ident Loc
type Memory = ReaderT Env IO
-- Stack size and resulting code
type CompileRes = (Int, String)

compileProgram :: Program -> String -> IO String
compileProgram program name = do
	mainContent <- runReaderT (translateProgram program) empty
	return (".class public " ++ name ++ "\n" ++
			".super java/lang/Object\n" ++
			".method public <init>()V\n" ++
			"  aload_0\n" ++
			"  invokespecial java/lang/Object/<init>()V\n" ++
			"  return\n" ++
			".end method\n" ++
			".method public static main([Ljava/lang/String;)V\n" ++ mainContent ++
			"\nreturn\n" ++
			".end method\n")

translateProgram :: Program -> Memory String
translateProgram (Prog stmts) = do
	results <- mapM (translateStmt) stmts
	let (stackSize, result) = Prelude.foldl (
			\(resSize, resCode) (size, code) -> (max resSize size, resCode ++ code)) 
		(0, "") results
	env <- ask
	let localsSize = (size env) + 1
	return $ ".limit stack " ++ show stackSize ++ "\n.limit locals " ++ show localsSize ++ "\n" ++ result

translateStmt :: Stmt -> Memory CompileRes
translateStmt stmt = do
	return (0, "TODO")