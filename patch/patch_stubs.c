#include <stdio.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/threads.h>

#include <SDL2/SDL.h>
#include <SDL2/SDL_video.h>

static value g_cb;

void patch_sdl_resize_event_watch(void* data, SDL_Event *e) {
    CAMLparam0();
    // According to SDL docs, an event watch can run in a separate thread.
    // According to OCaml interfacing with C manual, we must register threads
    // that wish to interact with the runtime.
    caml_c_thread_register();
    if (e->type == SDL_WINDOWEVENT) {
        if (e->window.event == SDL_WINDOWEVENT_RESIZED) {
            /* int w = e->window.data1; */
            /* int h = e->window.data2; */
            /* SDL_Log("SDL_WINDOWEVENT_RESIZED %ix%i\n", w, h); */
            SDL_Window* win = SDL_GetWindowFromID(e->window.windowID);
            caml_acquire_runtime_system();
            caml_callback(g_cb, Val_unit);
            caml_release_runtime_system();
        }
    }
    CAMLreturn0;
}

// SDL does not send resize events untill the user is done resizing (ie. releases the mouse button)
// See: https://bugzilla.libsdl.org/show_bug.cgi?id=2077
// We abuse an event watch, which *does* capture resize events as they happen, and short circuit
// any wait_event/poll_event calls by calling our callback (typically a draw function).
// This happens WHILE the OCaml thread is blocking on wait_event, so extreme care must be taken.
//
// TODO: Address this with upstream, SDL should deffinitely return from wait_event for resizing events.
void patch_sdl_listen_for_resize_event(value cb) {
    CAMLparam1(cb);
    caml_register_global_root(&g_cb);
    g_cb = cb;
    SDL_AddEventWatch(patch_sdl_resize_event_watch, NULL);
    CAMLreturn0;
}
