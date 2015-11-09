all: insc_jvm insc_llvm
	
insc_jvm: MainJVM.hs CompilerJVM.hs
	ghc MainJVM.hs -i../parser -o ../insc_jvm

insc_llvm: MainLLVM.hs CompilerLLVM.hs
	ghc MainLLVM.hs -i../parser -o ../insc_llvm

clean:
	rm -rf *.o *.hi