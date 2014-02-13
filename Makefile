all:
	# -cflags -w,@A-4-33-41-42-43-34-44
	corebuild -cflags -g,-ccopt,-g,-ccopt,-I${CURDIR} nacl_stubs.o
	corebuild -cflags -w,@A-4-33-41-42-43-34-44 -j 8 -cflags -g,-ccopt,-g \
		-yaccflags --explain,--dump -use-menhir \
		-pkg core_extended -pkg re2 -pkg termbox \
		-lflags -cclib,-lsodium,nacl_stubs.o \
		cli.native

clean:
	rm -rf _build
	rm -f cli.native
