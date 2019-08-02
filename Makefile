out: out.o commonmain.o
	#ld out.o commonmain.o libc -o out
	gcc -o out out.o commonmain.o

out.o: out.asm
	nasm -f macho64 -o out.o out.asm

out.asm: parse.rs
	rustc parse.rs -o parse
	./parse

commonmain.o: commonmain.c
	gcc -m64 -c -o commonmain.o commonmain.c

clean:
	rm -f out.o commonmain.o out

run: out
	./out
