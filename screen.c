#include "dnes.h"
#include <SDL3/SDL.h>
#include <SDL3/SDL_main.h>
/* We will use this renderer to draw into this window every frame. */
static SDL_Window *window = NULL;
static SDL_Renderer *renderer = NULL;

static const int WINDOW_WIDTH = 780;
static const int WINDOW_HEIGHT = 480;
static bool next_inst = false;

static void draw_string(int x, int y, const char *str, uint32_t color) {
  SDL_SetRenderDrawColor(renderer, (color >> 16) & 0xFF, (color >> 8) & 0xFF,
                         color & 0xFF, SDL_ALPHA_OPAQUE);
  SDL_RenderDebugText(renderer, x, y, str);
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

    start_addr += inst_byte_len;
    draw_string(x, y + i * 10, line_buf, COLOR_CYAN);
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
  return SDL_APP_CONTINUE; /* carry on with the program! */
}

/* This function runs once per frame, and is the heart of the program. */
SDL_AppResult SDL_AppIterate(void *appstate) {
  SDL_SetRenderDrawColor(renderer, 0, 0, 0,
                         SDL_ALPHA_OPAQUE); /* black, full alpha */
  SDL_RenderClear(renderer);                /* start with a blank canvas. */
  const bool *keys = SDL_GetKeyboardState(NULL);
  if (keys[SDL_SCANCODE_C] && !next_inst) {
    next_inst = true;
  } else if (keys[SDL_SCANCODE_C] && next_inst) {
    // nothing, wait for release
  } else if (!keys[SDL_SCANCODE_C] && next_inst) {
    SDL_Log("clock");
    do {
      dnes_clock();
    } while (!cpu_inst_done());
    do {
      dnes_clock();
    } while (cpu_inst_done());
    next_inst = false;
  }
  draw_cpu(516, 2);
  draw_code(516, 72, 26);
  SDL_RenderPresent(renderer); /* put it all on the screen! */

  return SDL_APP_CONTINUE; /* carry on with the program! */
}

/* This function runs once at shutdown. */
void SDL_AppQuit(void *appstate, SDL_AppResult result) {
  /* SDL will clean up the window/renderer for us. */
}
