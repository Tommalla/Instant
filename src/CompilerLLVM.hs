module CompilerLLVM where

import AbsInstant
import Data.Set
import Control.Monad.Except
import Control.Monad.State
import Data.List.Split

type Env = Set Ident
-- Next loc in register and env:
type Mem = (Int, Env)
type Memory = StateT Mem IO

compileProgram :: Program -> String -> IO String
compileProgram program name = do
	(mainContent, _) <- runStateT (translateProgram program) (0, empty)
	return ("@.str = private unnamed_addr constant [4 x i8] c\"%d\\0A\\00\", align 1\n" ++
			"declare i32 @printf(i8*, ...) #1\n" ++ 
			"define i32 @main() #0 {\n" ++ mainContent ++
			"\nret i32 0\n}")

translateProgram :: Program -> Memory String
translateProgram _ = return "TODO"