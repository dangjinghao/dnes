#include "dnes.h"
static byte_t controller_state[2];
static byte_t controller[2];

static inline addr_t ctrl_real_addr(addr_t addr) { return (addr & 0x0001); }
static void ctrl_write(addr_t addr, byte_t data) {
  (void)data;
  controller_state[ctrl_real_addr(addr)] = controller[ctrl_real_addr(addr)];
}

static byte_t ctrl_read(addr_t addr, bool read_only) {
  (void)read_only;
  byte_t data = (controller_state[ctrl_real_addr(addr)] & 0x80) > 0;
  controller_state[ctrl_real_addr(addr)] <<= 1;
  return data;
}

void ctrl_register(struct bus *bus) {
  struct bus_regparam param = {.read = ctrl_read, .write = ctrl_write};
  bus_register(bus, 0x4016, 0x4017, &param);
}

void ctrl_set_input(byte_t player, enum ctrl_button b, bool pressed) {
  if (pressed)
    controller[player] |= (1 << b);
  else
    controller[player] &= ~(1 << b);
}

void ctrl_reset() {
  controller_state[0] = 0x00;
  controller_state[1] = 0x00;
  controller[0] = 0x00;
  controller[1] = 0x00;
}

void ctrl_clear_input(byte_t player) { controller[player] = 0x00; }