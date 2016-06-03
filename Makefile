all:
	# -cflags -w,@A-4-33-41-42-43-34-44
	corebuild -cflags -w,@A-4-33-41-42-43-34-44-45 -j 8 -cflags -g,-ccopt,-g \
		-yaccflags --explain,--dump -use-menhir \
		-pkg core_extended -pkg re2 -pkg termbox -pkg nacl -pkg extunix \
		cli.native

clean:
	rm -rf _build
	rm -f cli.native
