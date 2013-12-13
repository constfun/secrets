#!/bin/sh

corebuild -pkg re2 -pkg ctypes.foreign -pkg scrypt -lflags -cclib,-lsodium cli.native
