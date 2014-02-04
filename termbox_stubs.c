#include "common.h"

#include <termbox.h>


CAMLprim value tbstub_init() {

	return Val_int(tb_init());
}


CAMLprim value tbstub_width() {

	return Val_int(tb_width());
}


CAMLprim value tbstub_height() {

	return Val_int(tb_height());
}


void tbstub_set_clear_attributes(value caml_fg, value caml_bg) {

	CAMLparam2(caml_fg, caml_bg);

	tb_set_clear_attributes(Int_val(caml_fg), Int_val(caml_bg));

	CAMLreturn0;
}


void tbstub_set_cursor(value caml_cx, value caml_cy) {

	CAMLparam2(caml_cx, caml_cy);

	tb_set_cursor(Int_val(caml_cx), Int_val(caml_cy));

	CAMLreturn0;
}


void tbstub_change_cell(value caml_x, value caml_y, value caml_ch, value caml_fg, value caml_bg) {

	CAMLparam5(caml_x, caml_y, caml_ch, caml_fg, caml_bg);

	tb_change_cell(Int_val(caml_x), Int_val(caml_y), Int32_val(caml_ch), Int_val(caml_fg), Int_val(caml_bg));

	CAMLreturn0;
}


CAMLprim value tbstub_poll_event() {

	CAMLparam0();
	CAMLlocal1(caml_e);
	CAMLlocal2(caml_key_event, caml_ch);
	CAMLlocal3(caml_resize_event, caml_width, caml_height);

	struct tb_event e;
	tb_poll_event(&e);

	if( e.type == TB_EVENT_KEY ) {

		caml_key_event = caml_alloc(0, 3);
		Store_field(caml_key_event, 0, Val_bool(e.mod));
		Store_field(caml_key_event, 1, Val_int(e.key));
		caml_ch = caml_copy_int32(e.ch);
		Store_field(caml_key_event, 2, caml_ch);

		caml_e = caml_alloc(1, 0);
		Store_field(caml_e, 0, caml_key_event);
	}
	else {
		caml_resize_event = caml_alloc(0, 2);
		Store_field(caml_resize_event, 0, Val_int(e.w));
		Store_field(caml_resize_event, 1, Val_int(e.h));

		caml_e = caml_alloc(1, 1);
		Store_field(caml_e, 0, caml_resize_event);
	}


	CAMLreturn(caml_e);
}
