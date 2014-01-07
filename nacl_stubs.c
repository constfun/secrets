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
	mlen = crypto_secretbox_ZEROBYTES + dlen;

	unsigned char *pd = &Byte_u(caml_data, 0);

	// Message must be padded by crypto_secretbox_ZEROBYTES of zoroed out bytes.
	m = calloc(mlen, 1);
	check_mem(m);
	memcpy((m + crypto_secretbox_ZEROBYTES), pd, dlen);

	c = caml_stat_alloc(mlen);
	n = &Byte_u(caml_nonce, 0);
	k = &Byte_u(caml_key, 0);

	crypto_secretbox(c, m, mlen, n, k);

	clen = mlen - crypto_secretbox_BOXZEROBYTES;
	cyphertext = caml_alloc_string(clen);
	memcpy(&Byte_u(cyphertext, 0), (c + crypto_secretbox_BOXZEROBYTES), clen);

	CAMLreturn(cyphertext);
}

CAMLprim value nacl_secretbox_open(value caml_cyphertext, value caml_nonce, value caml_key) {

	CAMLparam3(caml_cyphertext, caml_nonce, caml_key);
	CAMLlocal1(decrypted_data);

	unsigned char *c = NULL;
	unsigned char *m = NULL;
	unsigned char *n = NULL;
	unsigned char *k = NULL;
	size_t dlen = 0;
	size_t cyphertext_len = 0;
	size_t clen = 0;

	cyphertext_len = caml_string_length(caml_cyphertext);
	clen = crypto_secretbox_BOXZEROBYTES + cyphertext_len;
	m = caml_stat_alloc(clen);

	c = calloc(clen, 1);
	check_mem(c);
	memcpy((c + crypto_secretbox_BOXZEROBYTES), String_val(caml_cyphertext), cyphertext_len);

	n = &Byte_u(caml_nonce, 0);
	k = &Byte_u(caml_key, 0);

	int res = crypto_secretbox_open(m, c, clen, n, k);
	if( res == -1 ) {
		caml_raise_out_of_memory();
	}

	dlen = clen - crypto_secretbox_ZEROBYTES;
	decrypted_data = caml_alloc_string(dlen);
	memcpy(String_val(decrypted_data), (m + crypto_secretbox_ZEROBYTES), dlen);

	CAMLreturn(decrypted_data);
}
