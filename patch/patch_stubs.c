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
#include <caml/threads.h>

#include <SDL2/SDL.h>
#include <SDL2/SDL_video.h>

static value g_cb;

void eventFilter(void* data, SDL_Event *e )
{
    CAMLparam0();
    /* CAMLparam1(cb); */

    int res = caml_c_thread_register();
    printf("reg-thread %i\n", res);
    fflush(stdout);

    if (e->type == SDL_WINDOWEVENT) {
        if (e->window.event == SDL_WINDOWEVENT_RESIZED) {
            int w = e->window.data1;
            int h = e->window.data2;
            SDL_Log("SDL_WINDOWEVENT_RESIZED %ix%i\n", w, h);
            SDL_Window* win = SDL_GetWindowFromID(e->window.windowID);
            /* SDL_SetWindowSize(win, w, h); */
            /* caml_callback2(cb, Val_int(w), Val_int(h)); */
            caml_acquire_runtime_system();
            caml_callback(g_cb, Val_unit);
            caml_release_runtime_system();
        }
    }
    CAMLreturn0;
}

void tsdl_patch(value cb) {
    CAMLparam1(cb);
    caml_register_global_root(&g_cb);
    g_cb = cb;
    SDL_AddEventWatch(eventFilter, NULL);
    CAMLreturn0;
}
