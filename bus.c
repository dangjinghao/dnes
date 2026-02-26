#include "dnes.h"
#include <stdio.h>
#include <stdlib.h>

static inline const char *bus_name(const struct bus *bus) {
  if (!bus || !bus->name || bus->name[0] == '\0') {
    return "<unnamed-bus>";
  }
  return bus->name;
}

static inline addr_t bus_mask_addr(const struct bus *bus, addr_t addr) {
  return addr & bus->mask;
}

static void bus_must_valid(struct bus *bus) {
  if (bus->dev_count <= 1) {
    return;
  }

  for (size_t i = 0; i + 1 < bus->dev_count; i++) {
    if (bus->devices[i].end >= bus->devices[i + 1].start) {
      errorfln(
          "[%s] Conflicted registered device in register functions: %s & %s",
          bus_name(bus), bus->devices[i].reg_func_name,
          bus->devices[i + 1].reg_func_name);
      exit(1);
    }
  }
  for (size_t i = 0; i < bus->dev_count; i++) {
    printf("[%s] Registered device: %s at address range %#06X - %#06X\n",
           bus_name(bus), bus->devices[i].reg_func_name, bus->devices[i].start,
           bus->devices[i].end);
  }
}

static int bus_addr_cmp(const void *a, const void *b) {
  const struct bus_device *da = a;
  const struct bus_device *db = b;
  if (da->start < db->start) {
    return -1;
  }
  if (da->start > db->start) {
    return 1;
  }
  return 0;
}

static struct bus_device *bus_fetch_device(struct bus *bus, addr_t addr) {
  for (size_t i = 0; i < bus->dev_count; i++) {
    if (bus->devices[i].start <= addr && bus->devices[i].end >= addr) {
      return &bus->devices[i];
    }
  }
  return NULL;
}

void bus_init(struct bus *bus, const char *name, addr_t mask) {
  bus->dev_count = 0;
  bus->mask = mask;
  bus->name = name;
}

void bus_register_2(struct bus *bus, addr_t start, addr_t end,
                    struct bus_regparam *p, const char *reg_func_name) {
  assert(start <= end);
  assert(end <= bus->mask);
  assert(p);
  assert(bus->dev_count < sizeof(bus->devices) / sizeof(bus->devices[0]) &&
         "Too many bus->devices registered.");
  bus->devices[bus->dev_count].device = *p;
  bus->devices[bus->dev_count].start = start;
  bus->devices[bus->dev_count].end = end;
  bus->devices[bus->dev_count].reg_func_name = reg_func_name;
  bus->dev_count += 1;
}

void bus_ready(struct bus *bus) {
  // sort the registered addresses
  qsort(bus->devices, bus->dev_count, sizeof(bus->devices[0]), bus_addr_cmp);
  // check whether those addresses conflict
  bus_must_valid(bus);
}

void bus_write(struct bus *bus, addr_t addr, byte_t data) {
  addr = bus_mask_addr(bus, addr);
  struct bus_device *d = bus_fetch_device(bus, addr);
  if (!d) {
    errorfln("[%s] Failed to write data %#04X at invalid address %#06X: "
             "Can't found valid device",
             bus_name(bus), data, addr);
    return;
  }
  if (!d->device.write) {
    errorfln("[%s] Failed to write data %#04X at invalid address %#06X: this "
             "device isn't writeable",
             bus_name(bus), data, addr);
    return;
  }
  d->device.write(addr, data);
}

byte_t bus_read(struct bus *bus, addr_t addr) {
  addr = bus_mask_addr(bus, addr);
  struct bus_device *d = bus_fetch_device(bus, addr);
  if (!d) {
    errorfln("[%s] Failed to read from invalid address %#06X: Can't found "
             "valid device",
             bus_name(bus), addr);
    return 0x00;
  }
  if (!d->device.read) {
    errorfln("[%s] Failed to read from invalid address %#06X: this device "
             "isn't readable",
             bus_name(bus), addr);
    return 0x00;
  }
  return d->device.read(addr, false);
}

byte_t bus_read_only(struct bus *bus, addr_t addr) {
  addr = bus_mask_addr(bus, addr);
  struct bus_device *d = bus_fetch_device(bus, addr);
  if (!d) {
    errorfln("[%s] Failed to read from invalid address: %#06X", bus_name(bus),
             addr);
    return 0x00;
  }
  return d->device.read(addr, true);
}
