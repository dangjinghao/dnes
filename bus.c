#include "dnes.h"
#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>

static struct bus_devi {
  addr_t start, end;
  struct bus_device device;
} devices[32];

static size_t dev_count = 0;

static bool bus_valid() {
  if (dev_count <= 1) {
    return true;
  }

  for (size_t i = 0; i + 1 < dev_count; i++) {
    if (devices[i].end >= devices[i + 1].start) {
      return false;
    }
  }
  return true;
}

static int bus_addr_cmp(const void *a, const void *b) {
  const struct bus_devi *da = a;
  const struct bus_devi *db = b;
  if (da->start < db->start) {
    return -1;
  }
  if (da->start > db->start) {
    return 1;
  }
  return 0;
}

static struct bus_devi *bus_fetch_device(addr_t addr) {
  for (size_t i = 0; i < dev_count; i++) {
    if (devices[i].start <= addr && devices[i].end >= addr) {
      return &devices[i];
    }
  }
  return NULL;
}

void bus_register(addr_t start, addr_t end, struct bus_device *dev) {
  assert(start <= end);
  assert(dev);
  assert(dev_count < sizeof(devices) / sizeof(devices[0]) &&
         "Too many devices registered.");
  devices[dev_count].device = *dev;
  devices[dev_count].start = start;
  devices[dev_count].end = end;
  dev_count += 1;
}

void bus_ready() {
  // sort the registered addresses
  qsort(devices, dev_count, sizeof(devices[0]), bus_addr_cmp);
  // check whether those addresses conflict
  assert(bus_valid());
}

void bus_write(addr_t addr, byte_t data) {
  struct bus_devi *d = bus_fetch_device(addr);
  if (!d) {
    errorf("Failed to write data %#02X at invalid address %#04X: Can't found "
           "valid device\n",
           data, addr);
    return;
  }
  if (!d->device.write) {
    errorf("Failed to write data %#02X at invalid address %#04X: this device "
           "isn't writeable\n",
           data, addr);
    return;
  }
  d->device.write(addr, data);
}

byte_t bus_read(addr_t addr) {
  struct bus_devi *d = bus_fetch_device(addr);
  if (!d) {
    errorf(
        "Failed to read from invalid address %#04X: Can't found valid device\n",
        addr);
    return 0x00;
  }
  if (!d->device.read) {
    errorf("Failed to read from invalid address %#04X: this device "
           "isn't readable\n",
           addr);
    return 0x00;
  }
  return d->device.read(addr, false);
}

byte_t bus_read_only(addr_t addr) {
  struct bus_devi *d = bus_fetch_device(addr);
  if (!d) {
    errorf("Failed to read from invalid address: %#04X\n", addr);
    return 0x00;
  }
  return d->device.read(addr, true);
}
