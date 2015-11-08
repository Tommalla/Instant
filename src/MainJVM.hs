import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs )

import AbsInstant
import CompilerJVM
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
		Ok tree -> do
			-- TODO Compile
			putStrLn $ show tree


main :: IO ()
main = do
	inputFiles <- getArgs
	case inputFiles of
		[inputFile] -> compileProgram inputFile