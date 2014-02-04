#pragma once


#define CAML_NAME_SPACE


#include <string.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>


#define check(A) if(A) { goto error; }
#define check_mem(A) if(!A) { caml_raise_out_of_memory(); }

#define raise_error(E) \
	caml_raise_constant(*caml_named_value(E)); \
	CAMLreturn(1); // Never executed, but needed to prevent a compile time error.
