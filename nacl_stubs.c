#define CAML_NAME_SPACE

#include <string.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>

#include <sodium.h>

#define check_mem(A) if(!A) { caml_raise_out_of_memory(); }


CAMLprim value nacl_randombytes(value caml_size) {

	CAMLparam1(caml_size);
	CAMLlocal1(randbytes);

	size_t size = Unsigned_long_val(caml_size);

	randbytes = caml_alloc_string(size);
	randombytes_buf(&Byte_u(randbytes, 0), size);

	CAMLreturn(randbytes);
}

CAMLprim value nacl_secretbox(value caml_data, value caml_nonce, value caml_key) {

	CAMLparam3(caml_data, caml_nonce, caml_key);
	CAMLlocal1(cyphertext);

	unsigned char *c = NULL;
	unsigned char *m = NULL;
	unsigned char *n = NULL;
	unsigned char *k = NULL;
	size_t mlen = 0;
	size_t dlen = 0;
	size_t clen = 0;

	dlen = caml_string_length(caml_data);
	mlen = dlen + crypto_secretbox_ZEROBYTES;

	// Message must be padded by crypto_secretbox_ZEROBYTES of zoroed out bytes.
	m = calloc(mlen, 1);
	check_mem(m);
	memcpy(String_val(caml_data), (m + crypto_secretbox_ZEROBYTES), dlen);

	c = caml_stat_alloc(mlen);
	n = &Byte_u(caml_nonce, 0);
	k = &Byte_u(caml_key, 0);

	crypto_secretbox(c, m, mlen, n, k);

	clen = mlen - crypto_secretbox_BOXZEROBYTES;
	cyphertext = caml_alloc_string(clen);
	memcpy(c, String_val(cyphertext), clen);

	CAMLreturn(cyphertext);
}
