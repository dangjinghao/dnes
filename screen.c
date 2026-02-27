#include "dnes.h"
#include <SDL3/SDL.h>
#include <SDL3/SDL_main.h>
#include <SDL3/SDL_render.h>
#include <SDL3/SDL_scancode.h>
#include <SDL3/SDL_surface.h>
#include <stdio.h>

static const int WINDOW_WIDTH = 780;
static const int WINDOW_HEIGHT = 480;
static const double NES_NTSC_FPS = 60.0988;
static const double SLOW_FRAME_TOLERANCE = 1.05;
static const int PERF_LOG_INTERVAL_FRAMES = 120;

/* We will use this renderer to draw into this window every frame. */
static SDL_Window *window = NULL;
static SDL_Renderer *renderer = NULL;
static SDL_Texture *screen_texture = NULL;
static SDL_Texture *pattern_textures[2] = {NULL, NULL};

static bool emu_run = true;
static byte_t selected_palette = 0;
static bool show_oam = false;
static bool show_debug_info = false;

static struct frame_profiler {
  double acc_input;
  double acc_ui;
  double acc_screen_draw;
  double acc_present;
  double acc_emu;
  double acc_total;
  int frame_count;
} frame_profiler;

static double frame_seconds_from_counter_delta(uint64_t counter_delta,
                                               uint64_t counter_freq) {
  if (counter_freq == 0) {
    return 0.0;
  }
  return (double)counter_delta / (double)counter_freq;
}

static double frame_seconds_between(uint64_t start_counter,
                                    uint64_t end_counter,
                                    uint64_t counter_freq) {
  if (end_counter < start_counter) {
    return 0.0;
  }
  return frame_seconds_from_counter_delta(end_counter - start_counter,
                                          counter_freq);
}

static void frame_profiler_record(double input_seconds, double ui_seconds,
                                  double screen_draw_seconds,
                                  double present_seconds, double emu_seconds,
                                  double total_seconds) {
  frame_profiler.acc_input += input_seconds;
  frame_profiler.acc_ui += ui_seconds;
  frame_profiler.acc_screen_draw += screen_draw_seconds;
  frame_profiler.acc_present += present_seconds;
  frame_profiler.acc_emu += emu_seconds;
  frame_profiler.acc_total += total_seconds;
  frame_profiler.frame_count++;

  if (frame_profiler.frame_count < PERF_LOG_INTERVAL_FRAMES) {
    return;
  }

  const double divisor = (double)frame_profiler.frame_count;
  SDL_Log("Perf avg(%d): total=%.3fms input=%.3f ui=%.3f "
          "screen=%.3f present=%.3f emu=%.3f",
          frame_profiler.frame_count,
          (frame_profiler.acc_total / divisor) * 1000.0,
          (frame_profiler.acc_input / divisor) * 1000.0,
          (frame_profiler.acc_ui / divisor) * 1000.0,
          (frame_profiler.acc_screen_draw / divisor) * 1000.0,
          (frame_profiler.acc_present / divisor) * 1000.0,
          (frame_profiler.acc_emu / divisor) * 1000.0);

  /* Update window title to show FPS (average over the interval). */
  double avg_fps = 0.0;
  if (frame_profiler.acc_total > 0.0) {
    avg_fps = divisor / frame_profiler.acc_total;
  }
  static char title_buf[128];
  snprintf(title_buf, sizeof(title_buf), "djh's NES emulator - FPS: %.2f",
           avg_fps);
  if (window != NULL) {
    SDL_SetWindowTitle(window, title_buf);
  }

  frame_profiler.acc_input = 0.0;
  frame_profiler.acc_ui = 0.0;
  frame_profiler.acc_screen_draw = 0.0;
  frame_profiler.acc_present = 0.0;
  frame_profiler.acc_emu = 0.0;
  frame_profiler.acc_total = 0.0;
  frame_profiler.frame_count = 0;
}

static bool frame_is_too_slow(double frame_seconds) {
  const double target_frame_seconds = 1.0 / NES_NTSC_FPS;
  return frame_seconds > (target_frame_seconds * SLOW_FRAME_TOLERANCE);
}

static void frame_limit_to_nes_fps(uint64_t frame_start_counter,
                                   uint64_t counter_freq) {
  if (counter_freq == 0) {
    return;
  }

  const uint64_t now_counter = SDL_GetPerformanceCounter();
  const uint64_t elapsed_counter = now_counter - frame_start_counter;
  const double elapsed_seconds =
      frame_seconds_from_counter_delta(elapsed_counter, counter_freq);
  const double target_frame_seconds = 1.0 / NES_NTSC_FPS;

  if (elapsed_seconds >= target_frame_seconds) {
    return;
  }

  const double remaining_seconds = target_frame_seconds - elapsed_seconds;
  const uint64_t remaining_ns = (uint64_t)(remaining_seconds * 1000000000.0);
  if (remaining_ns > 0) {
    SDL_DelayNS(remaining_ns);
  }
}

static void draw_string(int x, int y, const char *str,
                        struct ppu_color *ppu_color) {
  SDL_SetRenderDrawColor(renderer, ppu_color->r, ppu_color->g, ppu_color->b,
                         ppu_color->a);
  SDL_RenderDebugText(renderer, (float)x, (float)y, str);
}

static void fill_rect_ppu_color(int x, int y, int w, int h,
                                struct ppu_color *color) {
  SDL_SetRenderDrawColor(renderer, color->r, color->g, color->b, color->a);
  SDL_FRect rect = {(float)x, (float)y, (float)w, (float)h};
  SDL_RenderFillRect(renderer, &rect);
}

static void draw_rect(int x, int y, int w, int h, struct ppu_color *color) {
  SDL_SetRenderDrawColor(renderer, color->r, color->g, color->b, color->a);

  SDL_FRect rect = {(float)x, (float)y, (float)w, (float)h};
  SDL_RenderRect(renderer, &rect);
}

static void draw_screen(int x, int y) {
  // WARN: we assume the memory layout of ppu_screen_output is compatible with
  // the RGBA format of the texture, which is true for our current definition of
  // struct ppu_color, but may not be true if that struct is changed.
  SDL_UpdateTexture(screen_texture, NULL, (byte_t *)ppu_screen_output,
                    sizeof(ppu_screen_output[0]));
  SDL_FRect dst = {(float)x, (float)y, 512.0f, 480.0f};
  SDL_RenderTexture(renderer, screen_texture, NULL, &dst);
}

static void draw_pattern_table(int table, int x, int y) {
  // WARN: we assume the memory layout of ppu_pattern_table is compatible with
  // the RGBA format of the texture, which is true for our current definition of
  // struct ppu_color, but may not be true if that struct is changed.
  SDL_UpdateTexture(pattern_textures[table], NULL,
                    (byte_t *)ppu_pattern_table[table],
                    sizeof(ppu_pattern_table[table][0]));
  SDL_FRect dst = {(float)x, (float)y, 128.0f, 128.0f};
  SDL_RenderTexture(renderer, pattern_textures[table], NULL, &dst);
}

static void reset_display() {
  SDL_SetRenderDrawColor(renderer, 0, 0, 0,
                         SDL_ALPHA_OPAQUE); /* black, full alpha */
  SDL_RenderClear(renderer);                /* start with a blank canvas. */
}

static void draw_cpu(int x, int y) {
  draw_string(x, y, "STATUS:", COLOR_WHITE);
  draw_string(x + 64, y, "N", cpu_get_flag(FLAG_N) ? COLOR_GREEN : COLOR_RED);
  draw_string(x + 80, y, "V", cpu_get_flag(FLAG_V) ? COLOR_GREEN : COLOR_RED);
  draw_string(x + 96, y, "-", cpu_get_flag(FLAG_U) ? COLOR_GREEN : COLOR_RED);
  draw_string(x + 112, y, "B", cpu_get_flag(FLAG_B) ? COLOR_GREEN : COLOR_RED);
  draw_string(x + 128, y, "D", cpu_get_flag(FLAG_D) ? COLOR_GREEN : COLOR_RED);
  draw_string(x + 144, y, "I", cpu_get_flag(FLAG_I) ? COLOR_GREEN : COLOR_RED);
  draw_string(x + 160, y, "Z", cpu_get_flag(FLAG_Z) ? COLOR_GREEN : COLOR_RED);
  draw_string(x + 178, y, "C", cpu_get_flag(FLAG_C) ? COLOR_GREEN : COLOR_RED);
  char buf[128];
  snprintf(buf, sizeof(buf), "PC: $%04X", cpu_get_reg_PC());
  draw_string(x, y + 10, buf, COLOR_WHITE);
  snprintf(buf, sizeof(buf), "A: $%02X  [%d]", cpu_get_reg_A(),
           cpu_get_reg_A());
  draw_string(x, y + 20, buf, COLOR_WHITE);
  snprintf(buf, sizeof(buf), "X: $%02X  [%d]", cpu_get_reg_X(),
           cpu_get_reg_X());
  draw_string(x, y + 30, buf, COLOR_WHITE);
  snprintf(buf, sizeof(buf), "Y: $%02X  [%d]", cpu_get_reg_Y(),
           cpu_get_reg_Y());
  draw_string(x, y + 40, buf, COLOR_WHITE);
  snprintf(buf, sizeof(buf), "Stack P: $%02X", cpu_get_reg_STKP());
  draw_string(x, y + 50, buf, COLOR_WHITE);
}

static void draw_code(int x, int y, int lines) {
  addr_t start_addr = cpu_get_reg_PC();
  char line_buf[256];
  for (int i = 0; i < lines; i++) {
    size_t str_used_len = 0;
    size_t inst_byte_len = 0;
    cpu_disasm_code(start_addr, line_buf + str_used_len,
                    sizeof(line_buf) - str_used_len, &str_used_len,
                    &inst_byte_len);

    start_addr += (addr_t)inst_byte_len;
    draw_string(x, y + i * 10, line_buf, i == 0 ? COLOR_CYAN : COLOR_WHITE);
  }
}

static void draw_oam(int x, int y, int lines) {
  char line_buf[256];
  for (int i = 0; i < lines; i++) {
    snprintf(line_buf, sizeof(line_buf), "%02X: (%3d, %3d) ID: %02X AT: %02X",
             i, ppu_oam_start[i * 4 + 3], ppu_oam_start[i * 4 + 0],
             ppu_oam_start[i * 4 + 1], ppu_oam_start[i * 4 + 2]);
    draw_string(x, y + i * 10, line_buf, COLOR_WHITE);
  }
}

static void draw_palettes() {
  const int nSwatchSize = 6;
  for (byte_t p = 0; p < 8; p++)   // For each palette
    for (byte_t s = 0; s < 4; s++) // For each index
      fill_rect_ppu_color(516 + p * (nSwatchSize * 5) + s * nSwatchSize, 340,
                          nSwatchSize, nSwatchSize,
                          ppu_get_color_from_palette(p, s));

  // Draw selection reticule around selected palette
  draw_rect(516 + selected_palette * (nSwatchSize * 5) - 1, 339,
            (nSwatchSize * 4) + 2, nSwatchSize + 2, COLOR_WHITE);
}

/* This function runs once at startup. */
SDL_AppResult SDL_AppInit(void **appstate, int argc, char *argv[]) {
  (void)appstate;
  if (argc < 2) {
    SDL_Log("Empty arguments");
    return 1;
  }
  SDL_SetAppMetadata("djh's NES emulator", "0.1", "cloud.gugugu.dnes");

  if (!SDL_Init(SDL_INIT_VIDEO)) {
    SDL_Log("Couldn't initialize SDL: %s", SDL_GetError());
    return SDL_APP_FAILURE;
  }

  if (!SDL_CreateWindowAndRenderer("djh's NES emulator", WINDOW_WIDTH,
                                   WINDOW_HEIGHT, SDL_WINDOW_RESIZABLE, &window,
                                   &renderer)) {
    SDL_Log("Couldn't create window/renderer: %s", SDL_GetError());
    return SDL_APP_FAILURE;
  }
  SDL_SetRenderLogicalPresentation(renderer, WINDOW_WIDTH, WINDOW_HEIGHT,
                                   SDL_LOGICAL_PRESENTATION_LETTERBOX);

  screen_texture = SDL_CreateTexture(renderer, SDL_PIXELFORMAT_RGBA32,
                                     SDL_TEXTUREACCESS_STREAMING, 256, 240);
  SDL_SetTextureScaleMode(screen_texture, SDL_SCALEMODE_NEAREST);
  if (screen_texture == NULL) {
    SDL_Log("Couldn't create screen texture: %s", SDL_GetError());
    return SDL_APP_FAILURE;
  }

  pattern_textures[0] = SDL_CreateTexture(
      renderer, SDL_PIXELFORMAT_RGBA32, SDL_TEXTUREACCESS_STREAMING, 128, 128);
  SDL_SetTextureScaleMode(pattern_textures[0], SDL_SCALEMODE_NEAREST);

  pattern_textures[1] = SDL_CreateTexture(
      renderer, SDL_PIXELFORMAT_RGBA32, SDL_TEXTUREACCESS_STREAMING, 128, 128);
  SDL_SetTextureScaleMode(pattern_textures[1], SDL_SCALEMODE_NEAREST);

  if (pattern_textures[0] == NULL || pattern_textures[1] == NULL) {
    SDL_Log("Couldn't create pattern textures: %s", SDL_GetError());
    return SDL_APP_FAILURE;
  }

  dnes_insert_cartridge(argv[1]);
  dnes_reset();
  SDL_Log("Reset done");
  return SDL_APP_CONTINUE; /* carry on with the program! */
}

/* This function runs when a new event (mouse input, keypresses, etc) occurs. */
SDL_AppResult SDL_AppEvent(void *appstate, SDL_Event *event) {
  (void)appstate;
  if (event->type == SDL_EVENT_QUIT) {
    return SDL_APP_SUCCESS; /* end the program, reporting success to the OS. */
  }

  if (event->type == SDL_EVENT_KEY_UP) {
    switch (event->key.scancode) {
    case SDL_SCANCODE_C: {
      do {
        dnes_clock();
      } while (!cpu_inst_done());
      // CPU clock runs slower than system clock, so it may be
      // complete for additional system clock cycles. Drain
      // those out
      do {
        dnes_clock();
      } while (cpu_inst_done());
      break;
    }
    case SDL_SCANCODE_P: {
      selected_palette = (selected_palette + 1) % 8;
      break;
    }
    case SDL_SCANCODE_R: {
      dnes_reset();
      break;
    }
    case SDL_SCANCODE_F: {
      do {
        dnes_clock();
      } while (!ppu_frame_complete);
      do {
        dnes_clock();
      } while (!cpu_inst_done());
      SDL_Log("Frame complete");

      ppu_frame_complete = false;
      break;
    }
    case SDL_SCANCODE_SPACE: {
      emu_run = !emu_run;
      break;
    }
    case SDL_SCANCODE_O: {
      show_oam = !show_oam;
      break;
    }
    case SDL_SCANCODE_TAB: {
      show_debug_info = !show_debug_info;
      break;
    }
    default:
      break;
    }
  }
  return SDL_APP_CONTINUE; /* carry on with the program! */
}

static void detect_controller_input() {
  const bool *state = SDL_GetKeyboardState(NULL);
  ctrl_set_input(0, CTRL_UP, state[SDL_SCANCODE_UP]);
  ctrl_set_input(0, CTRL_DOWN, state[SDL_SCANCODE_DOWN]);
  ctrl_set_input(0, CTRL_LEFT, state[SDL_SCANCODE_LEFT]);
  ctrl_set_input(0, CTRL_RIGHT, state[SDL_SCANCODE_RIGHT]);
  ctrl_set_input(0, CTRL_A, state[SDL_SCANCODE_X]);
  ctrl_set_input(0, CTRL_B, state[SDL_SCANCODE_Z]);
  ctrl_set_input(0, CTRL_SELECT, state[SDL_SCANCODE_A]);
  ctrl_set_input(0, CTRL_START, state[SDL_SCANCODE_S]);
}

/* This function runs once per frame, and is the heart of the program. */
SDL_AppResult SDL_AppIterate(void *appstate) {
  (void)appstate;
  const uint64_t counter_freq = SDL_GetPerformanceFrequency();
  const uint64_t frame_start_counter = SDL_GetPerformanceCounter();
  uint64_t stage_start_counter = 0;
  uint64_t stage_end_counter = frame_start_counter;
  double input_seconds = 0;
  double screen_draw_seconds = 0;
  double emu_seconds = 0;
  double ui_seconds = 0;
  double present_seconds = 0;

#define collect_stage_perf(_s)                                                 \
  ctx_mgr(                                                                     \
      { stage_start_counter = stage_end_counter; },                            \
      {                                                                        \
        stage_end_counter = SDL_GetPerformanceCounter();                       \
        _s = frame_seconds_between(stage_start_counter, stage_end_counter,     \
                                   counter_freq);                              \
      })

  reset_display();

  collect_stage_perf(input_seconds) { detect_controller_input(); }

  collect_stage_perf(screen_draw_seconds) { draw_screen(0, 0); }

  collect_stage_perf(emu_seconds) {
    if (emu_run) {
      do {
        dnes_clock();
      } while (!ppu_frame_complete);
      ppu_frame_complete = false;
    }
  }
  collect_stage_perf(ui_seconds) {
    if (show_debug_info) {
      draw_cpu(516, 2);
      if (show_oam) {
        draw_oam(516, 72, 26);
      } else {
        draw_code(516, 72, 26);
      }
      draw_palettes();
      ppu_gen_pattern_table(0, selected_palette);
      ppu_gen_pattern_table(1, selected_palette);
      draw_pattern_table(0, 516, 348);
      draw_pattern_table(1, 516 + 130, 348);
    }
  }

  collect_stage_perf(present_seconds) {
    SDL_RenderPresent(renderer); /* put it all on the screen! */
  }

#undef collect_stage_perf

  const uint64_t frame_end_counter = SDL_GetPerformanceCounter();
  const uint64_t frame_counter_delta = frame_end_counter - frame_start_counter;
  const double frame_seconds =
      frame_seconds_from_counter_delta(frame_counter_delta, counter_freq);

  frame_profiler_record(input_seconds, ui_seconds, screen_draw_seconds,
                        present_seconds, emu_seconds, frame_seconds);

  if (frame_is_too_slow(frame_seconds)) {
    SDL_Log("Slow frame: %.3f ms (target %.3f ms) [input=%.3f ui=%.3f "
            "screen=%.3f present=%.3f emu=%.3f]",
            frame_seconds * 1000.0, (1000.0 / NES_NTSC_FPS),
            input_seconds * 1000.0, ui_seconds * 1000.0,
            screen_draw_seconds * 1000.0, present_seconds * 1000.0,
            emu_seconds * 1000.0);
  }

  frame_limit_to_nes_fps(frame_start_counter, counter_freq);
  return SDL_APP_CONTINUE; /* carry on with the program! */
}

/* This function runs once at shutdown. */
void SDL_AppQuit(void *appstate, SDL_AppResult result) {
  /* SDL will clean up the window/renderer for us. */
  (void)appstate;
  (void)result;
  if (screen_texture != NULL) {
    SDL_DestroyTexture(screen_texture);
    screen_texture = NULL;
  }
  for (int i = 0; i < 2; i++) {
    if (pattern_textures[i] != NULL) {
      SDL_DestroyTexture(pattern_textures[i]);
      pattern_textures[i] = NULL;
    }
  }
}
