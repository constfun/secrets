(define-module (gnu packages ocaml-extunix)
	       #:use-module (guix packages)
	       #:use-module (guix download)
	       #:use-module (guix build-system gnu)
	       #:use-module (guix build-system ocaml)
	       #:use-module (guix build-system dune)
	       #:use-module (guix licenses)
	       #:use-module (gnu packages)
	       #:use-module (gnu packages base)
	       #:use-module (gnu packages python)
	       #:use-module (gnu packages ocaml)
	       #:use-module (gnu packages crypto)
	       #:use-module (mygnu packages ocaml-extunix)
	       #:use-module (gnu packages gawk))

(package
  (name "secrets")
  (version "0.11.0")
  (source #f)
  (build-system dune-build-system)
  (arguments
   `(
     #:ocaml ,ocaml-4.07
     #:findlib ,ocaml4.07-findlib
     #:dune ,ocaml4.07-dune
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
     ("libsodium"  ,libsodium)
     ("coreutils"  ,coreutils)
     ))
  (home-page "")
  (synopsis "")
  (description "")
  (license #f))



    
