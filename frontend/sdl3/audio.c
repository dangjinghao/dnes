#include "dnes.h"
#include <SDL3/SDL.h>
#include <SDL3/SDL_audio.h>
#include <assert.h>

static SDL_AudioStream *stream = NULL;
void audio_init() {

  SDL_AudioSpec spec = {
      .freq = 44100,
      .format = SDL_AUDIO_F32,
      .channels = 1,
  };
  dnes_set_sample_frequency(44100);
  stream = SDL_OpenAudioDeviceStream(SDL_AUDIO_DEVICE_DEFAULT_PLAYBACK, &spec,
                                     NULL, NULL);
  if (stream == NULL) {
    SDL_LogError(SDL_LOG_CATEGORY_APPLICATION,
                 "Failed to open audio stream: %s", SDL_GetError());
    return;
  }
  // SDL_OpenAudioDeviceStream starts the device paused. You have to tell it to
  // start!
  SDL_ResumeAudioStreamDevice(stream);
}

void audio_output(float *samples, size_t sample_count) {
  assert(stream);
  if (!SDL_PutAudioStreamData(stream, samples,
                              (int)(sample_count * sizeof(float)))) {
    SDL_LogError(SDL_LOG_CATEGORY_APPLICATION,
                 "Failed to put audio stream data: %s", SDL_GetError());
  }
}

void audio_destroy() {
  SDL_DestroyAudioStream(stream);
  stream = NULL;
  dnes_set_sample_frequency(0);
}

void audio_play(double s) {
  if (stream == NULL) {
    return;
  }
  float sample = (float)s;
  if (!SDL_PutAudioStreamData(stream, &sample, sizeof(sample))) {
    SDL_LogError(SDL_LOG_CATEGORY_APPLICATION,
                 "Failed to put audio stream data: %s", SDL_GetError());
    return;
  }
}
