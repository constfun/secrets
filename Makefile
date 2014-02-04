all:
	corebuild -cflags -g,-ccopt,-g,-ccopt,-I${CURDIR} nacl_stubs.o
	corebuild -cflags -g,-ccopt,-g,-ccopt,-I${CURDIR} termbox_stubs.o
	corebuild -j 8 -cflags -g,-ccopt,-g \
		-yaccflags --explain,--dump -use-menhir \
		-pkg core_extended -pkg re2 \
		-lflags -cclib,-ltermbox,-cclib,-lsodium,nacl_stubs.o,termbox_stubs.o \
		cli.native

clean:
	rm -rf _build
	rm -f cli.native
