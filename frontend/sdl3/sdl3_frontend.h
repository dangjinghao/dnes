#ifndef _SDL_FRONTEND_H
#define _SDL_FRONTEND_H

/// audio.c
//

#include <stddef.h>
void audio_init();
void audio_output(float *samples, size_t sample_count);
void audio_destroy();
void audio_demo();

#endif