module CompilerJVM where

import Data.Map
import Control.Monad.Reader
import Control.Monad.Except

import AbsInstant
import ErrM
import LexInstant
import ParInstant
import PrintInstant

type Loc = Int
type Env = Map Ident Loc
type CompileRes = ReaderT Env IO

compileProgram :: Program -> String -> IO (String)
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

translateProgram :: Program -> CompileRes (String)
translateProgram program = do
	return "TODO"
