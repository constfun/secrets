#!/bin/sh

corebuild -pkg re2 -pkg ctypes.foreign -lflags -cclib,-lsodium secrets.native
