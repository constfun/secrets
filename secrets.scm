(define-module (secrets)
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
  #:use-module (gnu packages version-control)
  #:use-module (mygnu packages ocaml-extunix)
  #:use-module (gnu packages gawk))

(define-public secrets
  (package
    (name "secrets")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://gitlab.com/constfun/secrets/-/archive/master/secrets-master.tar.gz")
       (sha256
	(base32
         "0ng988frzs6z0c3wn32wf6dl3y5dazc9555zg4kxw1v779gnxqnq"))))
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
       ("libsodium"  ,libsodium)
       ;("coreutils"  ,coreutils)
       ;; ("git" ,git)
       ))
    (home-page "")
    (synopsis "")
    (description "")
    (license #f)))

`secrets



    
