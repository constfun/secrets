#define CAML_NAME_SPACE

#include <string.h>

#include <sodium.h>

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

#define raise_nacl_error raise_error("Nacl_error")


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
	CAMLlocal1(caml_cyphertext);

	int res = 0;
	size_t caml_data_len = 0;
	size_t caml_cyphertext_len = 0;

	unsigned char *c    = NULL;
	unsigned char *m    = NULL;
	size_t	       mlen = 0;
	unsigned char *n    = NULL;
	unsigned char *k    = NULL;

	// Message must be padded by crypto_secretbox_ZEROBYTES of zoroed out bytes.
	// We create a larger, zeroed, message buffer and copy our data string in there at an offset.
	// See: http://nacl.cr.yp.to/secretbox.html
	caml_data_len = caml_string_length(caml_data);
	mlen = crypto_secretbox_ZEROBYTES + caml_data_len;
	m = calloc(mlen, 1);
	check_mem(m);
	memcpy((m + crypto_secretbox_ZEROBYTES), String_val(caml_data), caml_data_len);

	c = caml_stat_alloc(mlen);
	n = &Byte_u(caml_nonce, 0);
	k = &Byte_u(caml_key, 0);

	res = crypto_secretbox(c, m, mlen, n, k);
	check(res);

	// Cyphertext is guranteed to have the first crypto_secretbox_BOXZEROBYTES zeroed.
	// We chop off those zeroed bytes and return only the cyphertext.
	caml_cyphertext_len = mlen - crypto_secretbox_BOXZEROBYTES;
	caml_cyphertext = caml_alloc_string(caml_cyphertext_len);
	memcpy(String_val(caml_cyphertext), (c + crypto_secretbox_BOXZEROBYTES), caml_cyphertext_len);

	caml_stat_free(c);
	free(m);

	CAMLreturn(caml_cyphertext);

error:
	if(c) { caml_stat_free(c); }
	if(m) { free(m); }

	raise_nacl_error
}

CAMLprim value nacl_secretbox_open(value caml_cyphertext, value caml_nonce, value caml_key) {

	CAMLparam3(caml_cyphertext, caml_nonce, caml_key);
	CAMLlocal1(caml_data);

	int res = 0;
	size_t caml_cyphertext_len = 0;
	size_t caml_data_len = 0;

	unsigned char *c    = NULL;
	size_t	       clen = 0;
	unsigned char *m    = NULL;
	unsigned char *n    = NULL;
	unsigned char *k    = NULL;

	// Cyphertext must be padded by crypto_secretbox_BOXZEROBYTES of zoroed out bytes.
	// We create a larger, zeroed, cyphertext buffer and copy our cyphertext string in there at an offset.
	// See: http://nacl.cr.yp.to/secretbox.html
	caml_cyphertext_len = caml_string_length(caml_cyphertext);
	clen = crypto_secretbox_BOXZEROBYTES + caml_cyphertext_len;
	c = calloc(clen, 1);
	check_mem(c);
	memcpy((c + crypto_secretbox_BOXZEROBYTES), String_val(caml_cyphertext), caml_cyphertext_len);

	m = caml_stat_alloc(clen);
	n = &Byte_u(caml_nonce, 0);
	k = &Byte_u(caml_key, 0);

	res = crypto_secretbox_open(m, c, clen, n, k);
	check(res);

	// Message is guranteed to have the first crypto_secretbox_ZEROBYTES zeroed.
	// We chop off those zeroed bytes and return only the data string.
	caml_data_len = clen - crypto_secretbox_ZEROBYTES;
	caml_data = caml_alloc_string(caml_data_len);
	memcpy(String_val(caml_data), (m + crypto_secretbox_ZEROBYTES), caml_data_len);

	caml_stat_free(m);
	free(c);

	CAMLreturn(caml_data);

error:
	if(m) { caml_stat_free(m); }
	if(c) { free(c); }

	raise_nacl_error
}
