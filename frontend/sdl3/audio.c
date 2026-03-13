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
}

void audio_demo() {
  assert(stream);

  const int sample_rate = 44100;
  const int tone_hz = 440;
  const int target_queue_samples = 2048;
  static float phase = 0.0f;

  while (SDL_GetAudioStreamQueued(stream) <
         (target_queue_samples * (int)sizeof(float))) {
    static float samples[512];

    for (size_t i = 0; i < SDL_arraysize(samples); i++) {
      samples[i] = SDL_sinf(phase);
      phase += (2.0f * SDL_PI_F * tone_hz) / sample_rate;
      if (phase >= 2.0f * SDL_PI_F) {
        phase -= 2.0f * SDL_PI_F;
      }
    }

    if (!SDL_PutAudioStreamData(stream, samples, sizeof(samples))) {
      SDL_LogError(SDL_LOG_CATEGORY_APPLICATION,
                   "Failed to put audio stream data: %s", SDL_GetError());
      return;
    }
  }
}