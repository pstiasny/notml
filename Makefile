COMPILER = target/debug/compiler

.PHONY: $(COMPILER)


out: out.o commonmain.o
	#ld out.o commonmain.o libc -o out
	gcc -g -o out out.o commonmain.o

out.o: out.asm
	nasm -g -f macho64 -F dwarf -o out.o out.asm

$(COMPILER):
	RUSTFLAGS="-D warnings" cargo build

out.asm: $(COMPILER) programs/simple
	$(COMPILER) < programs/simple

commonmain.o: commonmain.c
	gcc -g -m64 -c -o commonmain.o commonmain.c

clean:
	rm -f out.o commonmain.o out
	cargo clean

run: out
	./out
