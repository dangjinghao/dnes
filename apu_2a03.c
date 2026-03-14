#include "dnes.h"
#include <math.h>
#include <string.h>
static uint32_t frame_clock_counter = 0;
static uint32_t clock_counter = 0;
static bool use_raw_mode = false;
static byte_t length_table[] = {10, 254, 20,  2,  40, 4,  80, 6,  160, 8,  60,
                                10, 14,  12,  26, 14, 12, 16, 24, 18,  48, 20,
                                96, 22,  192, 24, 72, 26, 16, 28, 32,  30};

static double global_time = 0.0;

// square wave pulse channel 1
static bool pulse1_enable = false;
static bool pulse1_halt = false;
static double pulse1_sample = 0.0;
static double pulse1_output = 0.0;
static struct sequencer pulse1_seq;
static struct oscpulse pulse1_osc;
static struct envelope pulse1_env;
static struct length_counter pulse1_lc;
static struct sweeper pulse1_sweep;

// square wave pulse channel 2
static bool pulse2_enable = false;
static bool pulse2_halt = false;
static double pulse2_sample = 0.0;
static double pulse2_output = 0.0;
static struct sequencer pulse2_seq;
static struct oscpulse pulse2_osc;
static struct envelope pulse2_env;
static struct length_counter pulse2_lc;
static struct sweeper pulse2_sweep;

// noise channel
static bool noise_enable = false;
static bool noise_halt = false;
static double noise_sample = 0.0;
static double noise_output = 0.0;
static struct sequencer noise_seq;
static struct envelope noise_env;
static struct length_counter noise_lc;

// Sequencer Module
// ~~~~~~~~~~~~~~~~
// The purpose of the sequencer is to output a '1' after a given
// interval. It does this by counting down from a start value,
// when that value is < 0, it gets reset, and an internal "rotary"
// buffer is shifted. The nature of ths shifted pattern is different
// depending upon the channel, or module that requires sequencing.
// For example, the square wave channels simply rotate the preset
// sequence, but the noise channel needs to generate pseudo-random
// outputs originating from the preset sequence.
//
// Consider a square wave channel. A preset sequence of 01010101
// will output a 1 more freqently than 00010001, assuming we
// always output the LSB. The speed of this output is also
// governed by the timer counting down. The frequency is higher
// for small timer values, and lower for larger. Increasing
// the frequency of the output potentially increases the
// audible frequency. In fact, this is how the pulse channels
// fundamentally work. A "duty cycle" shape is loaded into the
// sequencer and the timer is used to vary the pitch, yielding
// notes.
struct sequencer {
  uint32_t sequence, new_sequence;
  uint16_t timer, reload;
  byte_t output;
};

static void sequencer_init(struct sequencer *s) { memset(s, 0, sizeof(*s)); }

static byte_t
sequencer_clock(struct sequencer *sequencer, bool enable,
                apu_sequencer_clock_manipulator_t func_manipulator) {
  if (enable) {
    sequencer->timer--;
    if (sequencer->timer == 0xFFFF) {
      sequencer->timer = sequencer->reload;
      func_manipulator(&sequencer->sequence);
      sequencer->output = sequencer->sequence & 0x01;
    }
  }
  return sequencer->output;
}

struct length_counter {
  byte_t counter;
};

static void length_counter_init(struct length_counter *l) {
  memset(l, 0, sizeof(*l));
}

static byte_t length_counter_clock(struct length_counter *length_counter,
                                   bool enable, bool halt) {
  if (!enable) {
    length_counter->counter = 0;
  } else if (!halt && length_counter->counter > 0) {
    length_counter->counter--;
  }
  return length_counter->counter;
}

struct envelope {
  bool start, disable;
  uint16_t divider_count, volume, output, decay_count;
};

static void envelope_init(struct envelope *e) { memset(e, 0, sizeof(*e)); }

static void envelope_clock(struct envelope *e, bool loop) {
  if (!e->start) {
    if (e->divider_count == 0) {
      e->divider_count = e->volume;
      if (e->decay_count == 0) {
        if (loop) {
          e->decay_count = 15;
        }
      } else {
        e->decay_count--;
      }
    } else {
      e->divider_count--;
    }
  } else {
    e->start = false;
    e->decay_count = 15;
    e->divider_count = e->volume;
  }

  if (e->disable) {
    e->output = e->volume;
  } else {
    e->output = e->decay_count;
  }
}

struct oscpulse {
  double freq, dutycycle, amplitude, harmonics, pi;
};

static void oscpulse_init(struct oscpulse *osc) {
  memset(osc, 0, sizeof(*osc));
  osc->amplitude = 1.0;
  osc->harmonics = 20;
}

static double oscpulse_sample_approxsin(double t) {
  double j = t * 0.15915494309189535; // 1/(2*pi)
  j = j - (int)j;
  return 20.785 * j * (j - 0.5) * (j - 1.0);
}

static double oscpulse_sample(struct oscpulse *osc, double t) {
  double a = 0, b = 0, p = osc->dutycycle * 2.0 * M_PI;
  for (double n = 1; n < osc->harmonics; n++) {
    double c = n * osc->freq * 2.0 * M_PI * t;
    a += -oscpulse_sample_approxsin(c) / n;
    b += -oscpulse_sample_approxsin(c - p * n) / n;

    // a += -sin(c) / n;
    // b += -sin(c-p*n) / n;
  }

  return (2.0 * osc->amplitude / M_PI) * (a - b);
}

struct sweeper {
  bool enabled, down, reload, mute;
  byte_t shift, timer, period;
  uint16_t change;
};

static void sweeper_track(struct sweeper *s, uint16_t *target) {
  if (s->enabled) {
    s->change = *target >> s->shift;
    s->mute = (*target < 8) || (*target >= 0x800);
  }
}

static bool sweeper_clock(struct sweeper *s, uint16_t *target, bool channel) {
  bool changed = false;

  if (s->timer == 0 && s->enabled && s->shift > 0 && !s->mute) {
    if (*target >= 8 && s->change < 0x07FF) {
      if (s->down) {
        *target -= s->change - channel;
      } else {
        *target += s->change;
      }
      changed = true;
    }
  }

  // if (enabled)
  {
    if (s->timer == 0 || s->reload) {
      s->timer = s->period;
      s->reload = false;
    } else {
      s->timer--;
    }
    s->mute = (*target < 8) || (*target >= 0x800);
  }
  return changed;
}

static byte_t apu_read(addr_t addr, bool read_only) {
  (void)addr;
  (void)read_only;
  return 0x00;
}

static void apu_write(addr_t addr, byte_t data) {
  (void)addr;
  (void)data;
}

static void apu_init() { noise_seq.sequence = 0xDBDB; }

void apu_register(struct bus *bus) {
  apu_init();
  struct bus_regparam p = {.read = apu_read, .write = apu_write};
  bus_register(bus, 0x4000, 0x4013, &p);
  bus_register(bus, 0x4015, 0x4015, &p);
}

void apu_clock() {}

void apu_reset() {}

double apu_get_output_sample() { return 0; }

uint16_t apu_pulse1_visual = 0, apu_pulse2_visual = 0, apu_noise_visual = 0,
         apu_triangle_visual = 0;
