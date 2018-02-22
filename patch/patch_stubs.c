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
    if (e->type == SDL_WINDOWEVENT) {
        printf("SDL_WINDOWEVENT %i\n", e->type);
        if (e->window.event == SDL_WINDOWEVENT_RESIZED) {
            int w = e->window.data1;
            int h = e->window.data2;
            SDL_Log("SDL_WINDOWEVENT_RESIZED %ix%i\n", w, h);
            SDL_Window* win = SDL_GetWindowFromID(e->window.windowID);
            /* win->surface_valid = SDL_FALSE; */
            /* SDL_SetWindowSize(win, w, h); */
            caml_callback2(cb, Val_int(w), Val_int(h));
            /* SDL_FlushEvent(SDL_WINDOWEVENT); */
            /* fflush(stdout); */
        }
    }
    return 1; // return 1 so all events are added to queue
}

CAMLprim value tsdl_patch(value cb) {
    CAMLparam1(cb);
    SDL_SetEventFilter(eventFilter, cb);
    return Val_unit;
}
