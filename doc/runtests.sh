for i in `seq 1 99`
do
	./insc_jvm doc/Instant/test$i.ins
	java -classpath doc/Instant test$i > jvm.out
	diff -sq -Bb jvm.out doc/Instant/test$i.eval || break	
	./insc_llvm doc/Instant/test$i.ins
	lli doc/Instant/test$i.bc > llvm.out
	diff -sq -Bb llvm.out doc/Instant/test$i.eval || break
done