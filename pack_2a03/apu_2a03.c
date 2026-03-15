#include "dnes.h"
#include <math.h>
#include <string.h>
static uint32_t frame_clock_counter = 0;
static uint32_t clock_counter = 0;
static bool use_raw_mode = false;

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
  double frequency, dutycycle, amplitude, harmonics, pi;
};

static void oscpulse_init(struct oscpulse *osc) {
  memset(osc, 0, sizeof(*osc));
  osc->amplitude = 1.0;
  osc->harmonics = 20;
}

static inline double oscpulse_sample_approxsin(double t) {
  double j = t * 0.15915494309189535; // 1/(2*pi)
  j = j - (int)j;
  return 20.785 * j * (j - 0.5) * (j - 1.0);
}

static double oscpulse_sample(struct oscpulse *osc, double t) {
  double a = 0, b = 0, p = osc->dutycycle * 2.0 * M_PI;
  for (double n = 1; n < osc->harmonics; n++) {
    double c = n * osc->frequency * 2.0 * M_PI * t;
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

static byte_t apu_read(addr_t addr, bool read_only) {
  byte_t data = 0x00;
  (void)read_only;
  if (addr == 0x4015) {
    //	data |= (pulse1_lc.counter > 0) ? 0x01 : 0x00;
    //	data |= (pulse2_lc.counter > 0) ? 0x02 : 0x00;
    //	data |= (noise_lc.counter > 0) ? 0x04 : 0x00;
  }

  return data;
}

void apu_write(addr_t addr, byte_t data) {
  switch (addr) {
  case 0x4000: {
    switch ((data & 0xC0) >> 6) {
    case 0x00: {
      pulse1_seq.new_sequence = 0b01000000;
      pulse1_osc.dutycycle = 0.125;
      break;
    }
    case 0x01: {
      pulse1_seq.new_sequence = 0b01100000;
      pulse1_osc.dutycycle = 0.250;
      break;
    }
    case 0x02: {
      pulse1_seq.new_sequence = 0b01111000;
      pulse1_osc.dutycycle = 0.500;
      break;
    }
    case 0x03: {
      pulse1_seq.new_sequence = 0b10011111;
      pulse1_osc.dutycycle = 0.750;
      break;
    }
    }
    pulse1_seq.sequence = pulse1_seq.new_sequence;
    pulse1_halt = (data & 0x20);
    pulse1_env.volume = (data & 0x0F);
    pulse1_env.disable = (data & 0x10);
    break;
  }
  case 0x4001: {
    pulse1_sweep.enabled = data & 0x80;
    pulse1_sweep.period = (data & 0x70) >> 4;
    pulse1_sweep.down = data & 0x08;
    pulse1_sweep.shift = data & 0x07;
    pulse1_sweep.reload = true;
    break;
  }
  case 0x4002: {
    pulse1_seq.reload = (pulse1_seq.reload & 0xFF00) | data;
    break;
  }
  case 0x4003: {
    pulse1_seq.reload =
        (uint16_t)(((data & 0x07)) << 8 | (pulse1_seq.reload & 0x00FF));
    pulse1_seq.timer = pulse1_seq.reload;
    pulse1_seq.sequence = pulse1_seq.new_sequence;
    pulse1_lc.counter = length_table[(data & 0xF8) >> 3];
    pulse1_env.start = true;
    break;
  }
  case 0x4004: {
    switch ((data & 0xC0) >> 6) {
    case 0x00: {
      pulse2_seq.new_sequence = 0b01000000;
      pulse2_osc.dutycycle = 0.125;
      break;
    }
    case 0x01: {
      pulse2_seq.new_sequence = 0b01100000;
      pulse2_osc.dutycycle = 0.250;
      break;
    }
    case 0x02: {
      pulse2_seq.new_sequence = 0b01111000;
      pulse2_osc.dutycycle = 0.500;
      break;
    }
    case 0x03: {
      pulse2_seq.new_sequence = 0b10011111;
      pulse2_osc.dutycycle = 0.750;
      break;
    }
    }
    pulse2_seq.sequence = pulse2_seq.new_sequence;
    pulse2_halt = (data & 0x20);
    pulse2_env.volume = (data & 0x0F);
    pulse2_env.disable = (data & 0x10);
    break;
  }
  case 0x4005: {
    pulse2_sweep.enabled = data & 0x80;
    pulse2_sweep.period = (data & 0x70) >> 4;
    pulse2_sweep.down = data & 0x08;
    pulse2_sweep.shift = data & 0x07;
    pulse2_sweep.reload = true;
    break;
  }
  case 0x4006: {
    pulse2_seq.reload = (pulse2_seq.reload & 0xFF00) | data;
    break;
  }

  case 0x4007: {
    pulse2_seq.reload =
        (uint16_t)(((data & 0x07)) << 8 | (pulse2_seq.reload & 0x00FF));
    pulse2_seq.timer = pulse2_seq.reload;
    pulse2_seq.sequence = pulse2_seq.new_sequence;
    pulse2_lc.counter = length_table[(data & 0xF8) >> 3];
    pulse2_env.start = true;
    break;
  }
  case 0x4008:
  case 0x4009:
  case 0x400A:
  case 0x400B: {
    // Triangle channel registers are not implemented yet.
    break;
  }

  case 0x400C: {
    noise_env.volume = (data & 0x0F);
    noise_env.disable = (data & 0x10);
    noise_halt = (data & 0x20);
    break;
  }
  // case 0x400D: unused
  case 0x400E: {
    switch (data & 0x0F) {
    case 0x00:
      noise_seq.reload = 0;
      break;
    case 0x01:
      noise_seq.reload = 4;
      break;
    case 0x02:
      noise_seq.reload = 8;
      break;
    case 0x03:
      noise_seq.reload = 16;
      break;
    case 0x04:
      noise_seq.reload = 32;
      break;
    case 0x05:
      noise_seq.reload = 64;
      break;
    case 0x06:
      noise_seq.reload = 96;
      break;
    case 0x07:
      noise_seq.reload = 128;
      break;
    case 0x08:
      noise_seq.reload = 160;
      break;
    case 0x09:
      noise_seq.reload = 202;
      break;
    case 0x0A:
      noise_seq.reload = 254;
      break;
    case 0x0B:
      noise_seq.reload = 380;
      break;
    case 0x0C:
      noise_seq.reload = 508;
      break;
    case 0x0D:
      noise_seq.reload = 1016;
      break;
    case 0x0E:
      noise_seq.reload = 2034;
      break;
    case 0x0F:
      noise_seq.reload = 4068;
      break;
    }
    break;
  }

  case 0x400F: {
    pulse1_env.start = true;
    pulse2_env.start = true;
    noise_env.start = true;
    noise_lc.counter = length_table[(data & 0xF8) >> 3];
    break;
  }
  case 0x4010:
  case 0x4011:
  case 0x4012:
  case 0x4013: {
    // DMC registers are not implemented yet.
    break;
  }
  // case 0x4014: OAM DMA register is implemented in dma.c
  case 0x4015: { // APU STATUS
    pulse1_enable = data & 0x01;
    pulse2_enable = data & 0x02;
    noise_enable = data & 0x04;
    break;
  }
  // case 0x4016: controller register is implemented in controller.c
  // case 0x4017: frame counter control register
  default:
    break;
  }
}

static void apu_init() {
  sequencer_init(&pulse1_seq);
  sequencer_init(&pulse2_seq);
  sequencer_init(&noise_seq);
  envelope_init(&pulse1_env);
  envelope_init(&pulse2_env);
  envelope_init(&noise_env);
  length_counter_init(&pulse1_lc);
  length_counter_init(&pulse2_lc);
  length_counter_init(&noise_lc);
  oscpulse_init(&pulse1_osc);
  oscpulse_init(&pulse2_osc);

  noise_seq.sequence = 0xDBDB;
}

void apu_register(struct bus *bus) {
  apu_init();
  struct bus_regparam p = {.read = apu_read, .write = apu_write};
  bus_register(bus, 0x4000, 0x4013, &p);
  bus_register(bus, 0x4015, 0x4015, &p);
}

static void apu_clock_update_channel(uint32_t *s) {
  // Shift right by 1 bit, wrapping around
  *s = ((*s & 0x0001) << 7) | ((*s & 0x00FE) >> 1);
}

static void apu_clock_update_noise_channel(uint32_t *s) {
  *s = (((*s & 0x0001) ^ ((*s & 0x0002) >> 1)) << 14) | ((*s & 0x7FFF) >> 1);
}

void apu_clock() {
  // Depending on the frame count, we set a flag to tell
  // us where we are in the sequence. Essentially, changes
  // to notes only occur at these intervals, meaning, in a
  // way, this is responsible for ensuring musical time is
  // maintained.
  bool bQuarterFrameClock = false;
  bool bHalfFrameClock = false;

  global_time += (0.3333333333 / 1789773);

  if (clock_counter % 6 == 0) {
    frame_clock_counter++;

    // 4-Step Sequence Mode
    if (frame_clock_counter == 3729) {
      bQuarterFrameClock = true;
    }

    if (frame_clock_counter == 7457) {
      bQuarterFrameClock = true;
      bHalfFrameClock = true;
    }

    if (frame_clock_counter == 11186) {
      bQuarterFrameClock = true;
    }

    if (frame_clock_counter == 14916) {
      bQuarterFrameClock = true;
      bHalfFrameClock = true;
      frame_clock_counter = 0;
    }

    // Update functional units

    // Quater frame "beats" adjust the volume envelope
    if (bQuarterFrameClock) {
      envelope_clock(&pulse1_env, pulse1_halt);
      envelope_clock(&pulse2_env, pulse2_halt);
      envelope_clock(&noise_env, noise_halt);
    }

    // Half frame "beats" adjust the note length and
    // frequency sweepers
    if (bHalfFrameClock) {
      length_counter_clock(&pulse1_lc, pulse1_enable, pulse1_halt);
      length_counter_clock(&pulse2_lc, pulse2_enable, pulse2_halt);
      length_counter_clock(&noise_lc, noise_enable, noise_halt);
      sweeper_clock(&pulse1_sweep, &pulse1_seq.reload, 0);
      sweeper_clock(&pulse2_sweep, &pulse2_seq.reload, 1);
    }

    //	if (bUseRawMode)
    {
      // Update Pulse1 Channel ================================
      sequencer_clock(&pulse1_seq, pulse1_enable, apu_clock_update_channel);

      //	pulse1_sample = (double)pulse1_seq.output;
    }
    // else
    {
      pulse1_osc.frequency =
          1789773.0 / (16.0 * (double)(pulse1_seq.reload + 1));
      pulse1_osc.amplitude = (double)(pulse1_env.output - 1) / 16.0;
      pulse1_sample = oscpulse_sample(&pulse1_osc, global_time);
      if (pulse1_lc.counter > 0 && pulse1_seq.timer >= 8 &&
          !pulse1_sweep.mute && pulse1_env.output > 2)
        pulse1_output += (pulse1_sample - pulse1_output) * 0.5;
      else
        pulse1_output = 0;
    }

    // if (bUseRawMode)
    {
      // Update Pulse1 Channel ================================
      sequencer_clock(&pulse2_seq, pulse2_enable, apu_clock_update_channel);

      //	pulse2_sample = (double)pulse2_seq.output;
    }
    //	else
    {
      pulse2_osc.frequency =
          1789773.0 / (16.0 * (double)(pulse2_seq.reload + 1));
      pulse2_osc.amplitude = (double)(pulse2_env.output - 1) / 16.0;
      pulse2_sample = oscpulse_sample(&pulse2_osc, global_time);

      if (pulse2_lc.counter > 0 && pulse2_seq.timer >= 8 &&
          !pulse2_sweep.mute && pulse2_env.output > 2)
        pulse2_output += (pulse2_sample - pulse2_output) * 0.5;
      else
        pulse2_output = 0;
    }

    sequencer_clock(&noise_seq, noise_enable, apu_clock_update_noise_channel);

    if (noise_lc.counter > 0 && noise_seq.timer >= 8) {
      noise_output =
          (double)noise_seq.output * ((double)(noise_env.output - 1) / 16.0);
    }

    if (!pulse1_enable)
      pulse1_output = 0;
    if (!pulse2_enable)
      pulse2_output = 0;
    if (!noise_enable)
      noise_output = 0;
  }

  // Frequency sweepers change at high frequency
  sweeper_track(&pulse1_sweep, &pulse1_seq.reload);
  sweeper_track(&pulse2_sweep, &pulse2_seq.reload);

  apu_pulse1_visual =
      (pulse1_enable && pulse1_env.output > 1 && !pulse1_sweep.mute)
          ? pulse1_seq.reload
          : 2047;
  apu_pulse2_visual =
      (pulse2_enable && pulse2_env.output > 1 && !pulse2_sweep.mute)
          ? pulse2_seq.reload
          : 2047;
  apu_noise_visual =
      (noise_enable && noise_env.output > 1) ? noise_seq.reload : 2047;

  clock_counter++;
}

void apu_reset() {}

double apu_get_output_sample() {
  if (use_raw_mode) {
    return (pulse1_sample - 0.5) * 0.5 + (pulse2_sample - 0.5) * 0.5;
  } else {
    return ((1.0 * pulse1_output) - 0.8) * 0.1 +
           ((1.0 * pulse2_output) - 0.8) * 0.1 +
           ((2.0 * (noise_output - 0.5))) * 0.1;
  }
}

uint16_t apu_pulse1_visual = 0, apu_pulse2_visual = 0, apu_noise_visual = 0,
         apu_triangle_visual = 0;
