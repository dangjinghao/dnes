// NES uses address $4017 for frame counter control(write) and joystick 2
// data(read)

#include "dnes.h"

void dev_4017_register(struct bus *bus) {
  struct bus_regparam param = {.read = ctrl_read, .write = apu_write};
  bus_register(bus, 0x4017, 0x4017, &param);
}