#include "dnes.h"
#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>

static bool bus_valid(struct bus *bus) {
  if (bus->dev_count <= 1) {
    return true;
  }

  for (size_t i = 0; i + 1 < bus->dev_count; i++) {
    if (bus->devices[i].end >= bus->devices[i + 1].start) {
      return false;
    }
  }
  return true;
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

void bus_register(struct bus *bus, addr_t start, addr_t end,
                  struct bus_regparam *p) {
  assert(start <= end);
  assert(p);
  assert(bus->dev_count < sizeof(bus->devices) / sizeof(bus->devices[0]) &&
         "Too many bus->devices registered.");
  bus->devices[bus->dev_count].device = *p;
  bus->devices[bus->dev_count].start = start;
  bus->devices[bus->dev_count].end = end;
  bus->dev_count += 1;
}

void bus_ready(struct bus *bus) {
  // sort the registered addresses
  qsort(bus->devices, bus->dev_count, sizeof(bus->devices[0]), bus_addr_cmp);
  // check whether those addresses conflict
  assert(bus_valid(bus));
}

void bus_write(struct bus *bus, addr_t addr, byte_t data) {
  struct bus_device *d = bus_fetch_device(bus, addr);
  if (!d) {
    errorfln("Failed to write data %#02X at invalid address %#04X: Can't found "
           "valid device",
           data, addr);
    return;
  }
  if (!d->device.write) {
    errorfln("Failed to write data %#02X at invalid address %#04X: this device "
           "isn't writeable",
           data, addr);
    return;
  }
  d->device.write(addr, data);
}

byte_t bus_read(struct bus *bus, addr_t addr) {
  struct bus_device *d = bus_fetch_device(bus, addr);
  if (!d) {
    errorfln(
        "Failed to read from invalid address %#04X: Can't found valid device",
        addr);
    return 0x00;
  }
  if (!d->device.read) {
    errorfln("Failed to read from invalid address %#04X: this device "
           "isn't readable",
           addr);
    return 0x00;
  }
  return d->device.read(addr, false);
}

byte_t bus_read_only(struct bus *bus, addr_t addr) {
  struct bus_device *d = bus_fetch_device(bus, addr);
  if (!d) {
    errorfln("Failed to read from invalid address: %#04X", addr);
    return 0x00;
  }
  return d->device.read(addr, true);
}
