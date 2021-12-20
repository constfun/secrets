(define-public secrets
	       (package
		 (name "secrets")
		 (version "1.0.0")
		 (source
		   (origin
		     (method url-fetch)
		     (uri "https://github.com/constfun/secrets/archive/f620d2c5a0723e17a4b0239bfdd081a8ff8c16d6.tar.gz")
		     (sha256
		       (base32
			 "08xvc6yp7hkw5cq69v1azqs7vh0vm4k2xl9wr2rz1vjiscp68anr"))))
		 ;; (outputs '("out" "lib" "bin"))
		 (build-system dune-build-system)
		 (arguments
		   `(
		     #:ocaml ,ocaml-4.07
		     #:findlib ,ocaml4.07-findlib
		     #:dune ,ocaml4.07-dune
		     #:tests? #f
		     #:build-flags `("--verbose")
		     ))
		 ;; (inputs
		 ;;  `(("xclip" ,xclip)))
		 (native-inputs
		   `(
		     ;; ("linux-libre-headers"  ,linux-libre-headers)
		     ("python"  ,python-2)
		     ("ocaml-dune"  ,ocaml4.07-dune)
		     ("ocaml-menhir"  ,ocaml4.07-menhir)
		     ("ocaml-core"  ,ocaml4.07-core)

		     ("ocaml-core-kernel"  ,ocaml4.07-core-kernel)
		     ("ocaml-ppxlib"  ,ocaml4.07-ppxlib)
		     ("ocaml-extunix"  ,ocaml4.07-extunix)
		     ("ocaml-re2"  ,ocaml4.07-re2)
		     ("ocaml-qrc"  ,ocaml4.07-qrc)
		     ("libsodium"  ,libsodium)
		     ;("coreutils"  ,coreutils)
		     ;; ("git" ,git)
		     ))
		 (home-page "")
		 (synopsis "")
		 (description "")
		 (license #f)))
