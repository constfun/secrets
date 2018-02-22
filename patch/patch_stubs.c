#include <stdio.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/signals.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/intext.h>
#include <caml/bigarray.h>

#include <SDL2/SDL.h>
#include <SDL2/SDL_video.h>


int c = 0;
int eventFilter(void* cb, SDL_Event *e )
{
    if( e->type == 512) {
        /* SDL_SetVideoMode(e->resize.w,e->resize.h,0,SDL_ANYFORMAT | SDL_RESIZABLE); */
        printf("Hello World! %i\n", e->type);
        caml_callback(cb, Val_unit);
        fflush(stdout);
    }
    return 1; // return 1 so all events are added to queue
}

CAMLprim value tsdl_patch(value cb) {
    CAMLparam1(cb);
    printf("Hello World! %i\n", c++);
    fflush(stdout);
    SDL_SetEventFilter(eventFilter, cb);
    return Val_unit;
}
