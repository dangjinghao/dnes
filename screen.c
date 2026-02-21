#include "dnes.h"
#include <SDL3/SDL.h>
#include <SDL3/SDL_main.h>
#include <SDL3/SDL_render.h>
/* We will use this renderer to draw into this window every frame. */
static SDL_Window *window = NULL;
static SDL_Renderer *renderer = NULL;

static void draw_string(int x, int y, const char *str, uint32_t color) {
  SDL_SetRenderDrawColor(renderer, color_extract_red(color),
                         color_extract_green(color), color_extract_blue(color),
                         color_extract_alpha(color));
  SDL_RenderDebugText(renderer, x, y, str);
}

static void fill_rect_SDL_color(int x, int y, int w, int h,
                                struct SDL_Color *color) {
  SDL_SetRenderDrawColor(renderer, color->r, color->g, color->b, color->a);
  SDL_FRect rect = {x, y, w, h};
  SDL_RenderFillRect(renderer, &rect);
}

static void fill_rect(int x, int y, int w, int h, uint32_t color) {
  fill_rect_SDL_color(x, y, w, h,
                      &(struct SDL_Color){color_extract_red(color),
                                          color_extract_green(color),
                                          color_extract_blue(color),
                                          color_extract_alpha(color)});
}

static void draw_rect(int x, int y, int w, int h, uint32_t color) {
  SDL_SetRenderDrawColor(renderer, color_extract_red(color),
                         color_extract_green(color), color_extract_blue(color),
                         color_extract_alpha(color));
  SDL_FRect rect = {x, y, w, h};
  SDL_RenderRect(renderer, &rect);
}

static void draw_screen(int x, int y) {
  for (int row = 0; row < 240; row++) {
    for (int col = 0; col < 256; col++) {
      fill_rect_SDL_color(x + col * 2, y + row * 2, 2, 2,
                          &ppu_screen_output[row][col]);
    }
  }
}

static void draw_pattern_table(int table, int x, int y) {
  for (int row = 0; row < 128; row++) {
    for (int col = 0; col < 128; col++) {
      fill_rect_SDL_color(x + col, y + row, 2, 2,
                          &ppu_pattern_table[table][row][col]);
    }
  }
}
static const int WINDOW_WIDTH = 780;
static const int WINDOW_HEIGHT = 480;
static bool emu_run = false;
static byte_t selected_palette = 0;

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

    start_addr += inst_byte_len;
    draw_string(x, y + i * 10, line_buf, i == 0 ? COLOR_CYAN : COLOR_WHITE);
  }
}

/* This function runs once at startup. */
SDL_AppResult SDL_AppInit(void **appstate, int argc, char *argv[]) {
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

  dnes_insert_cartridge(argv[1]);
  dnes_reset();
  SDL_Log("Reset done");
  return SDL_APP_CONTINUE; /* carry on with the program! */
}

/* This function runs when a new event (mouse input, keypresses, etc) occurs. */
SDL_AppResult SDL_AppEvent(void *appstate, SDL_Event *event) {
  if (event->type == SDL_EVENT_QUIT) {
    return SDL_APP_SUCCESS; /* end the program, reporting success to the OS. */
  }

  if (event->type == SDL_EVENT_KEY_UP) {
    switch (event->key.scancode) {
    case SDL_SCANCODE_C: {
      // SDL_Log("clock");
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
      SDL_Log("TODO: start/pause running");
      break;
    }
    default:
      break;
    }
  }
  return SDL_APP_CONTINUE; /* carry on with the program! */
}

/* This function runs once per frame, and is the heart of the program. */
SDL_AppResult SDL_AppIterate(void *appstate) {
  SDL_SetRenderDrawColor(renderer, 0, 0, 0,
                         SDL_ALPHA_OPAQUE); /* black, full alpha */
  SDL_RenderClear(renderer);                /* start with a blank canvas. */

  draw_cpu(516, 2);
  draw_code(516, 72, 13);

  // Draw Palettes & Pattern Tables
  // ==============================================
  const int nSwatchSize = 6;
  for (byte_t p = 0; p < 8; p++)   // For each palette
    for (byte_t s = 0; s < 4; s++) // For each index
      fill_rect_SDL_color(516 + p * (nSwatchSize * 5) + s * nSwatchSize, 340,
                          nSwatchSize, nSwatchSize,
                          ppu_get_color_from_palette(p, s));

  // Draw selection reticule around selected palette
  draw_rect(516 + selected_palette * (nSwatchSize * 5) - 1, 339,
            (nSwatchSize * 4) + 2, nSwatchSize + 2, COLOR_WHITE);
  draw_screen(0, 0);

  ppu_gen_pattern_table(0, selected_palette);
  ppu_gen_pattern_table(1, selected_palette);

  draw_pattern_table(0, 516, 348);
  draw_pattern_table(1, 516 + 130, 348);

  SDL_RenderPresent(renderer); /* put it all on the screen! */

  return SDL_APP_CONTINUE; /* carry on with the program! */
}

/* This function runs once at shutdown. */
void SDL_AppQuit(void *appstate, SDL_AppResult result) {
  /* SDL will clean up the window/renderer for us. */
}
