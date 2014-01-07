all:
	corebuild -cflags -g,-ccopt,-g nacl_stubs.o
	corebuild -cflags -g,-ccopt,-g -pkg core_extended -pkg re2 -lflags -cclib,-lsodium,nacl_stubs.o cli.native

clean:
	rm -rf _build
	rm -f cli.native
