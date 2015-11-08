all:
	cd parser && $(MAKE)
	cd src && $(MAKE)

clean:
	cd parser && $(MAKE) clean
	cd src && $(MAKE) clean
	rm -rf insc_jvm insc_llvm
