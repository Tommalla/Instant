import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs )

import AbsInstant
import CompilerJVM (compileProgram)
import ErrM
import LexInstant
import ParInstant
import PrintInstant


compileProgram :: String -> IO ()
compileProgram inputFile = do
	code <- readFile inputFile
	let tokens = myLexer code
	case pProgram tokens of
		Bad errorStr -> do
			putStrLn "ERROR TODO"
		Ok program -> do
			res <- (CompilerJVM.compileProgram program "foo")
			putStrLn res


main :: IO ()
main = do
	inputFiles <- getArgs
	case inputFiles of
		[inputFile] -> Main.compileProgram inputFile