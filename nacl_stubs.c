#define CAML_NAME_SPACE

#include <string.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>

#include <sodium.h>


CAMLprim value nacl_randombytes(value caml_size) {

	CAMLparam1(caml_size);
	CAMLlocal1(randbytes);

	size_t size = Unsigned_long_val(caml_size);

	randbytes = caml_alloc_string(size);
	randombytes_buf(&Byte_u(randbytes, 0), size);

	CAMLreturn(randbytes);
}
