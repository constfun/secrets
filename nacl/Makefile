all:
	# Comple just the stubs into an object file.
	ocamlfind ocamlc -c nacl_stubs.c -ccopt -fPIC -o nacl_stubs.o

	# Make the dynamic and static libraries containing the stubs.
	ocamlfind ocamlmklib -lsodium nacl_stubs.o -o nacl_stubs

	# Make the cmi and cmo files.
	ocamlfind ocamlc -c nacl.mli nacl.ml
	# Make the bytecode library.
	ocamlfind ocamlc -a -dllib -lnacl_stubs -cclib -lnacl_stubs -cclib -lsodium nacl.cmo -o nacl.cma

	# Make the cmx file.
	ocamlopt -c nacl.ml
	# Make the native version of the library.
	ocamlopt -a -cclib -lnacl_stubs -cclib -lsodium nacl.cmx -o nacl.cmxa
	# Make a version of the library that supports being loaded by Dynlink.
	ocamlopt -shared nacl.cmx -o nacl.cmxs

install:
	ocamlfind install nacl META *.cmi *.cma *.cmx *.cmxa *.cmxs *.a *.so

uninstall:
	ocamlfind remove nacl

.PHONY: docs
docs:
	rm -rf docs
	mkdir docs
	ocamlfind ocamldoc -html -d docs nacl.mli

clean:
	rm -f *.cmi *.cmxa *.cmxs *.cma *.cmx *.cmo *.o *.so *.a
