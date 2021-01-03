#!/bin/sh

guix pack -RR -S /sec=bin/sec -L ~/projects/myguix --with-source=secrets@1.0.0=$(pwd) secrets
