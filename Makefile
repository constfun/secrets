all:
	#ocamlfind ocamlopt nacl_stubs.c
	corebuild nacl_stubs.o
	corebuild -lflag -verbose -pkg core_extended -pkg re2 -pkg ctypes.foreign -pkg scrypt -lflags -cclib,-lsodium,nacl_stubs.o cli.native

clean:
	rm -rf _build
