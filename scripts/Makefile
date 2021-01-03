build:
	guix build -L ~/projects/myguix --with-source=secrets@1.0.0=$(pwd) secrets

pack:
	guix pack -RR -S /sec=bin/sec -L ~/projects/myguix --with-source=secrets@1.0.0=$(pwd) secrets

size:
	guix size
