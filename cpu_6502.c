#include "dnes.h"
#include <stdio.h>

static struct {
  byte_t P; // Status register
  byte_t A;
  byte_t X;
  byte_t Y;
  byte_t STKP;
  addr_t PC;
  struct bus *bus;
  addr_t addr_abs;
  addr_t addr_rel;
  byte_t opcode;
  // current inst remaining cpu.cycles
  byte_t cycles;
  byte_t fetched;
} cpu;

static const addr_t stack_base = 0x0100;

// Addressing Modes
static bool IMP();
static bool IMM();
static bool ZP0();
static bool ZPX();
static bool ZPY();
static bool REL();
static bool ABS();
static bool ABX();
static bool ABY();
static bool IND();
static bool IZX();
static bool IZY();
// cpu.opcodes
static bool ADC();
static bool AND();
static bool ASL();
static bool BCC();
static bool BCS();
static bool BEQ();
static bool BIT();
static bool BMI();
static bool BNE();
static bool BPL();
static bool BRK();
static bool BVC();
static bool BVS();
static bool CLC();
static bool CLD();
static bool CLI();
static bool CLV();
static bool CMP();
static bool CPX();
static bool CPY();
static bool DEC();
static bool DEX();
static bool DEY();
static bool EOR();
static bool INC();
static bool INX();
static bool INY();
static bool JMP();
static bool JSR();
static bool LDA();
static bool LDX();
static bool LDY();
static bool LSR();
static bool NOP();
static bool ORA();
static bool PHA();
static bool PHP();
static bool PLA();
static bool PLP();
static bool ROL();
static bool ROR();
static bool RTI();
static bool RTS();
static bool SBC();
static bool SEC();
static bool SED();
static bool SEI();
static bool STA();
static bool STX();
static bool STY();
static bool TAX();
static bool TAY();
static bool TSX();
static bool TXA();
static bool TXS();
static bool TYA();
// Invalid instruction trap
static bool XXX();

struct inst {
  const char *op_name;
  bool (*operate)();
  const char *addr_mode_name;
  bool (*addr_mode)();
  byte_t cycles;
};

#define GEN_INST(OP, AM, CY) {#OP, OP, #AM, AM, CY}
#define GEN_XXX {"???", XXX, "IMP", IMP, 2}

static struct inst inst_lookup[] = {
    // 00 ~ 0F
    GEN_INST(BRK, IMP, 7),
    GEN_INST(ORA, IZX, 6),
    GEN_XXX,
    GEN_XXX,
    GEN_XXX,
    GEN_INST(ORA, ZP0, 3),
    GEN_INST(ASL, ZP0, 5),
    GEN_XXX,
    GEN_INST(PHP, IMP, 3),
    GEN_INST(ORA, IMM, 2),
    // this addr mode is `Accum`, but it's very similar with imp
    GEN_INST(ASL, IMP, 2),
    GEN_XXX,
    GEN_XXX,
    GEN_INST(ORA, ABS, 4),
    GEN_INST(ASL, ABS, 6),
    GEN_XXX,
    // 10 ~ 1F
    GEN_INST(BPL, REL, 2),
    GEN_INST(ORA, IZY, 5),
    GEN_XXX,
    GEN_XXX,
    GEN_XXX,
    GEN_INST(ORA, ZPX, 4),
    GEN_INST(ASL, ZPX, 6),
    GEN_XXX,
    GEN_INST(CLC, IMP, 2),
    GEN_INST(ORA, ABY, 4),
    GEN_XXX,
    GEN_XXX,
    GEN_XXX,
    GEN_INST(ORA, ABX, 4),
    GEN_INST(ASL, ABX, 7),
    GEN_XXX,
    // 20 ~ 2F
    GEN_INST(JSR, ABS, 6),
    GEN_INST(AND, IZX, 6),
    GEN_XXX,
    GEN_XXX,
    GEN_INST(BIT, ZP0, 3),
    GEN_INST(AND, ZP0, 3),
    GEN_INST(ROL, ZP0, 5),
    GEN_XXX,
    GEN_INST(PLP, IMP, 4),
    GEN_INST(AND, IMM, 2),
    // this addr mode is `Accum`, but it's very similar with imp
    GEN_INST(ROL, IMP, 2),
    GEN_XXX,
    GEN_INST(BIT, ABS, 4),
    GEN_INST(AND, ABS, 4),
    GEN_INST(ROL, ABS, 6),
    GEN_XXX,
    // 30 ~ 3F
    GEN_INST(BMI, REL, 2),
    GEN_INST(AND, IZY, 5),
    GEN_XXX,
    GEN_XXX,
    GEN_XXX,
    GEN_INST(AND, ZPX, 4),
    GEN_INST(ROL, ZPX, 6),
    GEN_XXX,
    GEN_INST(SEC, IMP, 2),
    GEN_INST(AND, ABY, 4),
    GEN_XXX,
    GEN_XXX,
    GEN_XXX,
    GEN_INST(AND, ABX, 4),
    GEN_INST(ROL, ABX, 7),
    GEN_XXX,
    // 40 ~ 4F
    GEN_INST(RTI, IMP, 6),
    GEN_INST(EOR, IZX, 6),
    GEN_XXX,
    GEN_XXX,
    GEN_XXX,
    GEN_INST(EOR, ZP0, 3),
    GEN_INST(LSR, ZP0, 5),
    GEN_XXX,
    GEN_INST(PHA, IMP, 3),
    GEN_INST(EOR, IMM, 2),
    // this addr mode is `Accum`, but it's very similar with imp
    GEN_INST(LSR, IMP, 2),
    GEN_XXX,
    GEN_INST(JMP, ABS, 3),
    GEN_INST(EOR, ABS, 4),
    GEN_INST(LSR, ABS, 6),
    GEN_XXX,
    // 50 ~ 5F
    GEN_INST(BVC, REL, 2),
    GEN_INST(EOR, IZY, 5),
    GEN_XXX,
    GEN_XXX,
    GEN_XXX,
    GEN_INST(EOR, ZPX, 4),
    GEN_INST(LSR, ZPX, 6),
    GEN_XXX,
    GEN_INST(CLI, IMP, 2),
    GEN_INST(EOR, ABY, 4),
    GEN_XXX,
    GEN_XXX,
    GEN_XXX,
    GEN_INST(EOR, ABX, 4),
    GEN_INST(LSR, ABX, 7),
    GEN_XXX,
    // 60 ~ 6F
    GEN_INST(RTS, IMP, 6),
    GEN_INST(ADC, IZX, 6),
    GEN_XXX,
    GEN_XXX,
    GEN_XXX,
    GEN_INST(ADC, ZP0, 3),
    GEN_INST(ROR, ZP0, 5),
    GEN_XXX,
    GEN_INST(PLA, IMP, 4),
    GEN_INST(ADC, IMM, 2),
    // this addr mode is `Accum`, but it's very similar with imp
    GEN_INST(ROR, IMP, 2),
    GEN_XXX,
    GEN_INST(JMP, IND, 5),
    GEN_INST(ADC, ABS, 4),
    GEN_INST(ROR, ABS, 6),
    GEN_XXX,
    // 70 ~ 7F
    GEN_INST(BVS, REL, 2),
    GEN_INST(ADC, IZY, 5),
    GEN_XXX,
    GEN_XXX,
    GEN_XXX,
    GEN_INST(ADC, ZPX, 4),
    GEN_INST(ROR, ZPX, 6),
    GEN_XXX,
    GEN_INST(SEI, IMP, 2),
    GEN_INST(ADC, ABY, 4),
    GEN_XXX,
    GEN_XXX,
    GEN_XXX,
    GEN_INST(ADC, ABX, 4),
    GEN_INST(ROR, ABX, 7),
    GEN_XXX,
    // 80 ~ 8F
    GEN_XXX,
    GEN_INST(STA, IZX, 6),
    GEN_XXX,
    GEN_XXX,
    GEN_INST(STY, ZP0, 3),
    GEN_INST(STA, ZP0, 3),
    GEN_INST(STX, ZP0, 3),
    GEN_XXX,
    GEN_INST(DEY, IMP, 2),
    GEN_XXX,
    GEN_INST(TXA, IMP, 2),
    GEN_XXX,
    GEN_INST(STY, ABS, 4),
    GEN_INST(STA, ABS, 4),
    GEN_INST(STX, ABS, 4),
    GEN_XXX,
    // 90 ~ 9F
    GEN_INST(BCC, REL, 2),
    GEN_INST(STA, IZY, 6),
    GEN_XXX,
    GEN_XXX,
    GEN_INST(STY, ZPX, 4),
    GEN_INST(STA, ZPX, 4),
    GEN_INST(STX, ZPY, 4),
    GEN_XXX,
    GEN_INST(TYA, IMP, 2),
    GEN_INST(STA, ABY, 5),
    GEN_INST(TXS, IMP, 2),
    GEN_XXX,
    GEN_XXX,
    GEN_INST(STA, ABX, 5),
    GEN_XXX,
    GEN_XXX,
    // A0 ~ AF
    GEN_INST(LDY, IMM, 2),
    GEN_INST(LDA, IZX, 6),
    GEN_INST(LDX, IMM, 2),
    GEN_XXX,
    GEN_INST(LDY, ZP0, 3),
    GEN_INST(LDA, ZP0, 3),
    GEN_INST(LDX, ZP0, 3),
    GEN_XXX,
    GEN_INST(TAY, IMP, 2),
    GEN_INST(LDA, IMM, 2),
    GEN_INST(TAX, IMP, 2),
    GEN_XXX,
    GEN_INST(LDY, ABS, 4),
    GEN_INST(LDA, ABS, 4),
    GEN_INST(LDX, ABS, 4),
    GEN_XXX,
    // B0 ~ BF
    GEN_INST(BCS, REL, 2),
    GEN_INST(LDA, IZY, 5),
    GEN_XXX,
    GEN_XXX,
    GEN_INST(LDY, ZPX, 4),
    GEN_INST(LDA, ZPX, 4),
    GEN_INST(LDX, ZPY, 4),
    GEN_XXX,
    GEN_INST(CLV, IMP, 2),
    GEN_INST(LDA, ABY, 4),
    GEN_INST(TSX, IMP, 2),
    GEN_XXX,
    GEN_INST(LDY, ABX, 4),
    GEN_INST(LDA, ABX, 4),
    GEN_INST(LDX, ABY, 4),
    GEN_XXX,
    // C0 ~ CF
    GEN_INST(CPY, IMM, 2),
    GEN_INST(CMP, IZX, 6),
    GEN_XXX,
    GEN_XXX,
    GEN_INST(CPY, ZP0, 3),
    GEN_INST(CMP, ZP0, 3),
    GEN_INST(DEC, ZP0, 5),
    GEN_XXX,
    GEN_INST(INY, IMP, 2),
    GEN_INST(CMP, IMM, 2),
    GEN_INST(DEX, IMP, 2),
    GEN_XXX,
    GEN_INST(CPY, ABS, 4),
    GEN_INST(CMP, ABS, 4),
    GEN_INST(DEC, ABS, 6),
    GEN_XXX,
    // D0 ~ DF
    GEN_INST(BNE, REL, 2),
    GEN_INST(CMP, IZY, 5),
    GEN_XXX,
    GEN_XXX,
    GEN_XXX,
    GEN_INST(CMP, ZPX, 4),
    GEN_INST(DEC, ZPX, 6),
    GEN_XXX,
    GEN_INST(CLD, IMP, 2),
    GEN_INST(CMP, ABY, 4),
    GEN_XXX,
    GEN_XXX,
    GEN_XXX,
    GEN_INST(CMP, ABX, 4),
    GEN_INST(DEC, ABX, 7),
    GEN_XXX,
    // E0 ~ EF
    GEN_INST(CPX, IMM, 2),
    GEN_INST(SBC, IZX, 6),
    GEN_XXX,
    GEN_XXX,
    GEN_INST(CPX, ZP0, 3),
    GEN_INST(SBC, ZP0, 3),
    GEN_INST(INC, ZP0, 5),
    GEN_XXX,
    GEN_INST(INX, IMP, 2),
    GEN_INST(SBC, IMM, 2),
    GEN_INST(NOP, IMP, 2),
    GEN_XXX,
    GEN_INST(CPX, ABS, 4),
    GEN_INST(SBC, ABS, 4),
    GEN_INST(INC, ABS, 6),
    GEN_XXX,
    // F0 ~ FF
    GEN_INST(BEQ, REL, 2),
    GEN_INST(SBC, IZY, 5),
    GEN_XXX,
    GEN_XXX,
    GEN_XXX,
    GEN_INST(SBC, ZPX, 4),
    GEN_INST(INC, ZPX, 6),
    GEN_XXX,
    GEN_INST(SED, IMP, 2),
    GEN_INST(SBC, ABY, 4),
    GEN_XXX,
    GEN_XXX,
    GEN_XXX,
    GEN_INST(SBC, ABX, 4),
    GEN_INST(INC, ABX, 7),
    GEN_XXX,
};

static inline addr_t comb_addr(byte_t hi, byte_t lo) {
  return (addr_t)(((addr_t)hi << 8) | (addr_t)lo);
}

static inline bool is_byte_neg(byte_t B) { return (B & 0x80) != 0; }

static inline byte_t addr_hi(addr_t a) { return (byte_t)(a >> 8); }
static inline byte_t addr_lo(addr_t a) { return (byte_t)(a & 0xFF); }

static inline bool gen_flag(uint16_t f) { return !!f; }

static inline void cpu_set_flag(byte_t flag, bool v) {
  if (v)
    cpu.P |= flag;
  else
    cpu.P &= (byte_t)~flag;
}

static inline bool gen_status_C(uint16_t v) { return gen_flag(v > 0x00FF); }

static inline bool gen_status_Z(byte_t v) { return gen_flag(v == 0); }
static inline bool gen_status_N(byte_t v) { return gen_flag(is_byte_neg(v)); }

static inline void push_byte(byte_t B) {
  bus_write(cpu.bus, stack_base + (cpu.STKP--), B);
}

static inline void push_pc() {
  push_byte(addr_hi(cpu.PC));
  push_byte(addr_lo(cpu.PC));
}

static inline void push_status() { push_byte(cpu.P); }

static inline byte_t pop_byte() {
  return bus_read(cpu.bus, stack_base + (++cpu.STKP));
}

static inline addr_t pop_pc() {
  byte_t lo = pop_byte();
  byte_t hi = pop_byte();
  return comb_addr(hi, lo);
}

static byte_t fetch() {
  if (inst_lookup[cpu.opcode].addr_mode != IMP)
    cpu.fetched = bus_read(cpu.bus, cpu.addr_abs);

  return cpu.fetched;
}

/// Addressing Modes
// the return value means 'whether this addressing mode will increase the cycle'

static bool IMP() {
  // looks like it combined IMP and Accum addressing modes.
  cpu.fetched = cpu.A;
  return false;
}
static bool IMM() {
  cpu.addr_abs = cpu.PC++;
  return false;
}
// the data can be found in page 0
static bool ZP0() {
  cpu.addr_abs = bus_read(cpu.bus, cpu.PC++);
  cpu.addr_abs &= 0x00FF;
  return false;
}
static bool ZPX() {
  cpu.addr_abs = (bus_read(cpu.bus, cpu.PC++) + cpu.X);
  cpu.addr_abs &= 0x00FF;
  return false;
}
static bool ZPY() {
  cpu.addr_abs = (bus_read(cpu.bus, cpu.PC++) + cpu.Y);
  cpu.addr_abs &= 0x00FF;
  return false;
}
static bool REL() {
  cpu.addr_rel = bus_read(cpu.bus, cpu.PC++);
  // range: -128 ~ 127
  // if the readed rel address byte is negative,
  // we need to extend the signal bit
  if (is_byte_neg((byte_t)cpu.addr_rel))
    cpu.addr_rel |= 0xFF00;
  return false;
}
static bool ABS() {
  byte_t lo = bus_read(cpu.bus, cpu.PC++);
  byte_t hi = bus_read(cpu.bus, cpu.PC++);

  cpu.addr_abs = comb_addr(hi, lo);

  return false;
}
static bool ABX() {
  byte_t lo = bus_read(cpu.bus, cpu.PC++);
  byte_t hi = bus_read(cpu.bus, cpu.PC++);

  cpu.addr_abs = comb_addr(hi, lo);
  cpu.addr_abs += cpu.X;

  if (addr_hi(cpu.addr_abs) != hi) {
    // cross the page
    return true;
  }

  return false;
}
static bool ABY() {
  byte_t lo = bus_read(cpu.bus, cpu.PC++);
  byte_t hi = bus_read(cpu.bus, cpu.PC++);

  cpu.addr_abs = comb_addr(hi, lo);
  cpu.addr_abs += cpu.Y;

  if (addr_hi(cpu.addr_abs) != hi) {
    // cross the page
    return true;
  }

  return false;
}
// indirect addressing
static bool IND() {
  byte_t lo = bus_read(cpu.bus, cpu.PC++);
  byte_t hi = bus_read(cpu.bus, cpu.PC++);
  addr_t addr = comb_addr(hi, lo);

  if (lo == 0xFF) {
    // 6502 hardware bug WARN: An indirect JMP (xxFF) will fail because the MSB
    // will be cpu.fetched from address xx00 instead of page xx+1.
    lo = bus_read(cpu.bus, addr);
    // read xx00
    hi = bus_read(cpu.bus, addr & 0xFF00);
  } else {
    lo = bus_read(cpu.bus, addr);
    hi = bus_read(cpu.bus, addr + 1);
  }
  cpu.addr_abs = comb_addr(hi, lo);

  return false;
}
// indirect address of 0 page with X register
static bool IZX() {
  addr_t t = bus_read(cpu.bus, cpu.PC++);
  t = t + (addr_t)cpu.X;
  byte_t lo = bus_read(cpu.bus, t & 0x00FF);
  byte_t hi = bus_read(cpu.bus, (t + 1) & 0x00FF);
  cpu.addr_abs = comb_addr(hi, lo);

  return false;
}
static bool IZY() {
  addr_t t = bus_read(cpu.bus, cpu.PC++);
  byte_t lo = bus_read(cpu.bus, t & 0x00FF);
  byte_t hi = bus_read(cpu.bus, (t + 1) & 0x00FF);

  cpu.addr_abs = comb_addr(hi, lo);
  cpu.addr_abs += cpu.Y;

  if (addr_hi(cpu.addr_abs) != hi) {
    // cross the page
    return true;
  }

  return false;
}

/// Instructions
// the return means 'whether this instruction need the additional cpu.cycles'

// Addition with carry bit
static bool ADC() {
  // nestest will test ADC with decimal mode, if asserted, it will fail.
  // assert(!cpu_get_flag(FLAG_D));

  // get the data
  fetch();

  uint16_t result = (uint16_t)((uint16_t)cpu.A + (uint16_t)cpu.fetched +
                               (uint16_t)cpu_get_flag(FLAG_C));
  cpu_set_flag(FLAG_C, gen_status_C(result));
  cpu_set_flag(FLAG_Z, gen_status_Z((byte_t)result));

  // ref: olc6502 ADC comment
  // To assist us, the 6502 can set the overflow flag, if the result of the
  // addition has
  // wrapped around. V <- ~(A^M) & A^(A+M+C) :D lol, let's work out why!
  //
  // Let's suppose we have A = 30, M = 10 and C = 0
  //          A = 30 = 00011110
  //          M = 10 = 00001010+
  //     RESULT = 40 = 00101000
  //
  // Here we have not gone out of range. The resulting significant bit has not
  // changed. So let's make a truth table to understand when overflow has
  // occurred. Here I take the MSB of each component, where R is RESULT.
  //
  // A  M  R | V | A^R | A^M |~(A^M) |
  // 0  0  0 | 0 |  0  |  0  |   1   |
  // 0  0  1 | 1 |  1  |  0  |   1   |
  // 0  1  0 | 0 |  0  |  1  |   0   |
  // 0  1  1 | 0 |  1  |  1  |   0   |  so V = ~(A^M) & (A^R)
  // 1  0  0 | 0 |  1  |  1  |   0   |
  // 1  0  1 | 0 |  0  |  1  |   0   |
  // 1  1  0 | 1 |  1  |  0  |   1   |
  // 1  1  1 | 0 |  0  |  0  |   1   |
  //
  // We can see how the above equation calculates V, based on A, M and R. V was
  // chosen based on the following hypothesis:
  //       Positive Number + Positive Number = Negative Result -> Overflow
  //       Negative Number + Negative Number = Positive Result -> Overflow
  //       Positive Number + Negative Number = Either Result -> Cannot Overflow
  //       Positive Number + Positive Number = Positive Result -> OK!
  //       Negative Number + Negative Number = Negative Result -> OK!
  // SetFlag(V, (~((uint16_t)a ^ (uint16_t)cpu.fetched) &
  //            ((uint16_t)a ^ (uint16_t)temp)) & 0x0080);
  uint16_t tmp1 = ~((uint16_t)cpu.A ^ (uint16_t)cpu.fetched);
  uint16_t tmp2 = (uint16_t)cpu.A ^ (uint16_t)result;
  cpu_set_flag(FLAG_V, gen_flag((tmp1 & tmp2) & 0x0080));
  cpu_set_flag(FLAG_N, gen_status_N((byte_t)result));
  cpu.A = (byte_t)result;
  return true;
}

// Subtraction with carry bit
// in theory, A = A - M - borrow(prev bit)
// but in 6502, borrow = 1 - C
// to reuse the circuit
// A = A - M - (1-C)
//   = A + (-M) -1 + C
//   = A + (~M + 1) - 1 + C
//   = A + ~M + C
// A = A + ~M + C
static bool SBC() {
  // nestest will test ADC with decimal mode, if asserted, it will fail.
  // assert(!cpu_get_flag(FLAG_D));
  fetch();
  // ~M
  uint16_t M = ((uint16_t)cpu.fetched) ^ 0x00FF;
  uint16_t result = (uint16_t)((uint16_t)cpu.A + (uint16_t)M +
                               (uint16_t)cpu_get_flag(FLAG_C));
  cpu_set_flag(FLAG_C, gen_status_C(result));
  cpu_set_flag(FLAG_Z, gen_status_Z((byte_t)result));
  // Overflow: (+)A - (+)M = (-)
  //           (-)A - (-)M = (+)
  // SetFlag(V, (temp ^ (uint16_t)a) & (temp ^ value) & 0x0080);
  uint16_t tmp1 = (uint16_t)cpu.A ^ result;
  uint16_t tmp2 = (uint16_t)M ^ result;
  cpu_set_flag(FLAG_V, gen_flag((tmp1 & tmp2) & 0x0080));
  cpu_set_flag(FLAG_N, gen_status_N((byte_t)result));
  cpu.A = (byte_t)result;
  return true;
}

// bitwise logic and
static bool AND() {
  fetch();
  cpu.A &= cpu.fetched;
  cpu_set_flag(FLAG_Z, gen_status_Z(cpu.A));
  cpu_set_flag(FLAG_N, gen_status_N(cpu.A));
  return true;
}

static bool ASL() {
  fetch();
  uint16_t result = (uint16_t)(cpu.fetched << 1);
  cpu_set_flag(FLAG_C, gen_status_C(result));
  cpu_set_flag(FLAG_Z, gen_status_Z((byte_t)result));
  cpu_set_flag(FLAG_N, gen_status_N((byte_t)result));
  // asm: ASL A
  if (inst_lookup[cpu.opcode].addr_mode == IMP)
    cpu.A = (byte_t)result;
  else
    bus_write(cpu.bus, cpu.addr_abs, (byte_t)result);

  return false;
}
// branch if Carry clear
static bool BCC() {
  if (!cpu_get_flag(FLAG_C)) {
    cpu.cycles += 1;
    cpu.addr_abs = cpu.PC + cpu.addr_rel;
    // different page branch, +1 cycle
    if (addr_hi(cpu.addr_abs) != addr_hi(cpu.PC))
      cpu.cycles += 1;

    cpu.PC = cpu.addr_abs;
  }
  return false;
}
// branch if carry set
static bool BCS() {
  if (cpu_get_flag(FLAG_C)) {
    cpu.cycles += 1;
    cpu.addr_abs = cpu.PC + cpu.addr_rel;
    // different page branch, +1 cycle
    if (addr_hi(cpu.addr_abs) != addr_hi(cpu.PC))
      cpu.cycles += 1;

    cpu.PC = cpu.addr_abs;
  }
  return false;
}
// branch if equal
static bool BEQ() {
  if (cpu_get_flag(FLAG_Z)) {
    cpu.cycles += 1;
    cpu.addr_abs = cpu.PC + cpu.addr_rel;
    if (addr_hi(cpu.addr_abs) != addr_hi(cpu.PC))
      cpu.cycles += 1;

    cpu.PC = cpu.addr_abs;
  }

  return false;
}
static bool BIT() {
  fetch();
  uint16_t result = cpu.A & cpu.fetched;
  cpu_set_flag(FLAG_Z, gen_status_Z((byte_t)result));
  cpu_set_flag(FLAG_N, gen_status_N((byte_t)cpu.fetched));
  cpu_set_flag(FLAG_V, gen_flag(cpu.fetched & (1 << 6)));
  return false;
}
// branch if negative
static bool BMI() {
  if (cpu_get_flag(FLAG_N)) {
    cpu.cycles += 1;
    cpu.addr_abs = cpu.PC + cpu.addr_rel;

    if (addr_hi(cpu.addr_abs) != addr_hi(cpu.PC))
      cpu.cycles += 1;

    cpu.PC = cpu.addr_abs;
  }

  return false;
}
// branch if not equal
static bool BNE() {
  if (!cpu_get_flag(FLAG_Z)) {
    cpu.cycles += 1;
    cpu.addr_abs = cpu.PC + cpu.addr_rel;

    if (addr_hi(cpu.addr_abs) != addr_hi(cpu.PC))
      cpu.cycles += 1;

    cpu.PC = cpu.addr_abs;
  }

  return false;
}
// branch if positive
static bool BPL() {
  if (!cpu_get_flag(FLAG_N)) {
    cpu.cycles += 1;
    cpu.addr_abs = cpu.PC + cpu.addr_rel;

    if (addr_hi(cpu.addr_abs) != addr_hi(cpu.PC))
      cpu.cycles += 1;

    cpu.PC = cpu.addr_abs;
  }

  return false;
}
static bool BRK() {
  cpu.PC += 1;
  push_pc();
  cpu_set_flag(FLAG_B, true);
  cpu_set_flag(FLAG_U, true);
  push_status();
  cpu_set_flag(FLAG_B, false);
  // WARN: olc6502 set I status before pushing
  cpu_set_flag(FLAG_I, true);

  byte_t lo = bus_read(cpu.bus, 0xFFFE);
  byte_t hi = bus_read(cpu.bus, 0xFFFF);
  cpu.PC = comb_addr(hi, lo);
  return false;
}
// branch if overflow clear
static bool BVC() {
  if (!cpu_get_flag(FLAG_V)) {
    cpu.cycles += 1;
    cpu.addr_abs = cpu.PC + cpu.addr_rel;

    if (addr_hi(cpu.addr_abs) != addr_hi(cpu.PC))
      cpu.cycles += 1;

    cpu.PC = cpu.addr_abs;
  }

  return false;
}
// branch if overflow set
static bool BVS() {
  if (cpu_get_flag(FLAG_V)) {
    cpu.cycles += 1;
    cpu.addr_abs = cpu.PC + cpu.addr_rel;

    if (addr_hi(cpu.addr_abs) != addr_hi(cpu.PC))
      cpu.cycles += 1;

    cpu.PC = cpu.addr_abs;
  }

  return false;
}
static bool CLC() {
  cpu_set_flag(FLAG_C, false);
  return false;
}
static bool CLD() {
  cpu_set_flag(FLAG_D, false);
  return false;
}
static bool CLI() {
  cpu_set_flag(FLAG_I, false);
  return false;
}
static bool CLV() {
  cpu_set_flag(FLAG_V, false);
  return false;
}
static bool CMP() {
  fetch();
  uint16_t result = cpu.A - cpu.fetched;
  cpu_set_flag(FLAG_C, cpu.A >= cpu.fetched);
  cpu_set_flag(FLAG_Z, gen_status_Z((byte_t)result));
  cpu_set_flag(FLAG_N, gen_status_N((byte_t)result));
  return true;
}
static bool CPX() {
  fetch();
  uint16_t result = cpu.X - cpu.fetched;
  cpu_set_flag(FLAG_C, cpu.X >= cpu.fetched);
  cpu_set_flag(FLAG_Z, gen_status_Z((byte_t)result));
  cpu_set_flag(FLAG_N, gen_status_N((byte_t)result));
  return false;
}
static bool CPY() {
  fetch();
  uint16_t result = cpu.Y - cpu.fetched;
  cpu_set_flag(FLAG_C, cpu.Y >= cpu.fetched);
  cpu_set_flag(FLAG_Z, gen_status_Z((byte_t)result));
  cpu_set_flag(FLAG_N, gen_status_N((byte_t)result));
  return false;
}

static bool DEC() {
  fetch();
  uint16_t result = cpu.fetched - 1;
  bus_write(cpu.bus, cpu.addr_abs, (byte_t)result);
  cpu_set_flag(FLAG_Z, gen_status_Z((byte_t)result));
  cpu_set_flag(FLAG_N, gen_status_N((byte_t)result));
  return false;
}
static bool DEX() {
  cpu.X -= 1;
  cpu_set_flag(FLAG_Z, gen_status_Z(cpu.X));
  cpu_set_flag(FLAG_N, gen_status_N(cpu.X));
  return false;
}
static bool DEY() {
  cpu.Y -= 1;
  cpu_set_flag(FLAG_Z, gen_status_Z(cpu.Y));
  cpu_set_flag(FLAG_N, gen_status_N(cpu.Y));
  return false;
}
static bool EOR() {
  fetch();
  cpu.A ^= cpu.fetched;
  cpu_set_flag(FLAG_Z, gen_status_Z(cpu.A));
  cpu_set_flag(FLAG_N, gen_status_N(cpu.A));
  return false;
}
static bool INC() {
  fetch();
  uint16_t result = cpu.fetched + 1;
  bus_write(cpu.bus, cpu.addr_abs, (byte_t)result);
  cpu_set_flag(FLAG_Z, gen_status_Z((byte_t)result));
  cpu_set_flag(FLAG_N, gen_status_N((byte_t)result));
  return false;
}
static bool INX() {
  cpu.X += 1;
  cpu_set_flag(FLAG_Z, gen_status_Z(cpu.X));
  cpu_set_flag(FLAG_N, gen_status_N(cpu.X));
  return false;
}
static bool INY() {
  cpu.Y += 1;
  cpu_set_flag(FLAG_Z, gen_status_Z(cpu.Y));
  cpu_set_flag(FLAG_N, gen_status_N(cpu.Y));
  return false;
}
static bool JMP() {
  cpu.PC = cpu.addr_abs;
  return false;
}
static bool JSR() {
  // just be compatable with +1 in RTS
  cpu.PC -= 1;
  push_pc();
  cpu.PC = cpu.addr_abs;
  return false;
}
static bool LDA() {
  fetch();
  cpu.A = cpu.fetched;
  cpu_set_flag(FLAG_Z, gen_status_Z(cpu.A));
  cpu_set_flag(FLAG_N, gen_status_N(cpu.A));
  return true;
}
static bool LDX() {
  fetch();
  cpu.X = cpu.fetched;
  cpu_set_flag(FLAG_Z, gen_status_Z(cpu.X));
  cpu_set_flag(FLAG_N, gen_status_N(cpu.X));
  return true;
}
static bool LDY() {
  fetch();
  cpu.Y = cpu.fetched;
  cpu_set_flag(FLAG_Z, gen_status_Z(cpu.Y));
  cpu_set_flag(FLAG_N, gen_status_N(cpu.Y));
  return true;
}
static bool LSR() {
  fetch();
  cpu_set_flag(FLAG_C, gen_flag(cpu.fetched & 0x0001));
  uint16_t result = cpu.fetched >> 1;
  cpu_set_flag(FLAG_Z, gen_status_Z((byte_t)result));
  cpu_set_flag(FLAG_N, gen_status_N((byte_t)result));
  if (inst_lookup[cpu.opcode].addr_mode == IMP)
    cpu.A = (byte_t)result;
  else
    bus_write(cpu.bus, cpu.addr_abs, (byte_t)result);

  return false;
}
static bool NOP() {
  switch (cpu.opcode) {
  case 0x1C:
  case 0x3C:
  case 0x5C:
  case 0x7C:
  case 0xDC:
  case 0xFC:
    return true;
    break;
  }
  return 0;
}
static bool ORA() {
  fetch();
  cpu.A |= cpu.fetched;
  cpu_set_flag(FLAG_Z, gen_status_Z(cpu.A));
  cpu_set_flag(FLAG_N, gen_status_N(cpu.A));
  return true;
}
static bool PHA() {
  bus_write(cpu.bus, stack_base + (cpu.STKP--), cpu.A);
  return false;
}
static bool PLA() {
  cpu.A = pop_byte();
  cpu_set_flag(FLAG_Z, gen_status_Z(cpu.A));
  cpu_set_flag(FLAG_N, gen_status_N(cpu.A));
  return false;
}
static bool PHP() {
  cpu_set_flag(FLAG_B, true);
  cpu_set_flag(FLAG_U, true);
  push_status();
  cpu_set_flag(FLAG_B, false);
  return false;
}
static bool PLP() {
  cpu.P = pop_byte();
  // WARN: olc6502 doesn't reset B
  cpu_set_flag(FLAG_B, false);
  cpu_set_flag(FLAG_U, true);
  return false;
}
static bool ROL() {
  fetch();
  uint16_t result = (uint16_t)((cpu.fetched << 1) | cpu_get_flag(FLAG_C));
  cpu_set_flag(FLAG_C, gen_status_C(result));
  cpu_set_flag(FLAG_Z, gen_status_Z((byte_t)result));
  cpu_set_flag(FLAG_N, gen_status_N((byte_t)result));

  if (inst_lookup[cpu.opcode].addr_mode == IMP)
    cpu.A = (byte_t)result;
  else
    bus_write(cpu.bus, cpu.addr_abs, (byte_t)result);

  return false;
}
static bool ROR() {
  fetch();
  uint16_t result =
      (uint16_t)((cpu_get_flag(FLAG_C) << 7) | (cpu.fetched >> 1));
  cpu_set_flag(FLAG_C, (byte_t)(cpu.fetched & 0x01));
  cpu_set_flag(FLAG_Z, gen_status_Z((byte_t)result));
  cpu_set_flag(FLAG_N, gen_status_N((byte_t)result));
  if (inst_lookup[cpu.opcode].addr_mode == IMP)
    cpu.A = (byte_t)result;
  else
    bus_write(cpu.bus, cpu.addr_abs, (byte_t)result);

  return false;
}
static bool RTI() {
  cpu.P = pop_byte();
  cpu_set_flag(FLAG_B, false);
  // WARN: bit 5 reads back as 1 on a 6502; keep internal copy consistent
  // but olc6502 just reset it
  cpu_set_flag(FLAG_U, true);
  cpu.PC = pop_pc();
  return false;
}
static bool RTS() {
  cpu.PC = pop_pc();
  cpu.PC += 1;
  return false;
}
static bool SEC() {
  cpu_set_flag(FLAG_C, true);
  return false;
}
static bool SED() {
  cpu_set_flag(FLAG_D, true);
  return false;
}
static bool SEI() {
  cpu_set_flag(FLAG_I, true);
  return false;
}
static bool STA() {
  bus_write(cpu.bus, cpu.addr_abs, cpu.A);
  return false;
}
static bool STX() {
  bus_write(cpu.bus, cpu.addr_abs, cpu.X);
  return false;
}
static bool STY() {
  bus_write(cpu.bus, cpu.addr_abs, cpu.Y);
  return false;
}
static bool TAX() {
  cpu.X = cpu.A;
  cpu_set_flag(FLAG_Z, gen_status_Z(cpu.X));
  cpu_set_flag(FLAG_N, gen_status_N(cpu.X));
  return false;
}
static bool TAY() {
  cpu.Y = cpu.A;
  cpu_set_flag(FLAG_Z, gen_status_Z(cpu.Y));
  cpu_set_flag(FLAG_N, gen_status_N(cpu.Y));
  return false;
}
static bool TSX() {
  cpu.X = cpu.STKP;
  cpu_set_flag(FLAG_Z, gen_status_Z(cpu.X));
  cpu_set_flag(FLAG_N, gen_status_N(cpu.X));
  return false;
}
static bool TXA() {
  cpu.A = cpu.X;
  cpu_set_flag(FLAG_Z, gen_status_Z(cpu.A));
  cpu_set_flag(FLAG_N, gen_status_N(cpu.A));
  return false;
}
static bool TXS() {
  cpu.STKP = cpu.X;
  return false;
}
static bool TYA() {
  cpu.A = cpu.Y;
  cpu_set_flag(FLAG_Z, gen_status_Z(cpu.A));
  cpu_set_flag(FLAG_N, gen_status_N(cpu.A));
  return false;
}
// Invalid instruction trap
static bool XXX() {
  errorfln("Catch illegal cpu.opcode: %02X", cpu.opcode);
  return false;
}

void cpu_clock() {
  if (cpu.cycles == 0) {
    // previous inst is processed, fetch new one
    cpu.opcode = bus_read(cpu.bus, cpu.PC++);
    struct inst i = inst_lookup[cpu.opcode];
    cpu.cycles = i.cycles;

    bool additional_cycle = i.addr_mode();
    bool require_additional_cycle = i.operate();
    if (additional_cycle && require_additional_cycle)
      cpu.cycles += 1;
  }

  cpu.cycles -= 1;
}

void cpu_mount_mbus(struct bus *bus) { cpu.bus = bus; }

void cpu_reset() {
  cpu.A = 0;
  cpu.X = 0;
  cpu.Y = 0;
  cpu.STKP = 0xFD;
  cpu.P = 0;
  // this unused bit should always be 1 in the datasheet
  cpu_set_flag(FLAG_U, true);
  // WARN: olc6502 doesn't set I status
  cpu_set_flag(FLAG_I, true);
  cpu_set_flag(FLAG_D, false);
  cpu.addr_abs = 0xFFFC;
  byte_t lo = bus_read(cpu.bus, cpu.addr_abs);
  byte_t hi = bus_read(cpu.bus, cpu.addr_abs + 1);
  // 7 cpu.cycles in std reset manual, they are previous operations.

  // 8th cycle, JMP to the address
  cpu.PC = comb_addr(hi, lo);

  cpu.addr_rel = 0x0000;
  cpu.addr_abs = 0x0000;
  cpu.fetched = 0x00;

  cpu.cycles = 8;
}
void cpu_irq() {
  if (cpu_get_flag(FLAG_I)) {
    return;
  }
  push_pc();

  cpu_set_flag(FLAG_B, false);
  // bit 5 reads back as 1 on a 6502; keep stack copy consistent
  cpu_set_flag(FLAG_U, true);
  push_status();
  // WARN: wrong implementation in olc6502, the I flag should be set after push
  cpu_set_flag(FLAG_I, true);
  cpu.addr_abs = 0xFFFE;
  byte_t lo = bus_read(cpu.bus, cpu.addr_abs);
  byte_t hi = bus_read(cpu.bus, cpu.addr_abs + 1);
  cpu.PC = comb_addr(hi, lo);
  cpu.cycles = 7;
}

void cpu_nmi() {
  push_pc();
  cpu_set_flag(FLAG_B, false);
  // bit 5 reads back as 1 on a 6502; keep stack copy consistent
  cpu_set_flag(FLAG_U, true);
  push_status();
  // WARN: wrong implementation in olc6502, the I flag should be set after push
  cpu_set_flag(FLAG_I, true);
  cpu.addr_abs = 0xFFFA;
  byte_t lo = bus_read(cpu.bus, cpu.addr_abs);
  byte_t hi = bus_read(cpu.bus, cpu.addr_abs + 1);
  cpu.PC = comb_addr(hi, lo);
  cpu.cycles = 8;
}

bool cpu_inst_done() { return cpu.cycles == 0; }

bool cpu_get_flag(enum CPU_P_FLAGS flag) { return (cpu.P & flag) != 0; }

addr_t cpu_get_reg_PC() { return cpu.PC; }

byte_t cpu_get_reg_A() { return cpu.A; }

byte_t cpu_get_reg_X() { return cpu.X; }

byte_t cpu_get_reg_Y() { return cpu.Y; }

byte_t cpu_get_reg_STKP() { return cpu.STKP; }

// disasm the code at addr, and write the result to buf, return the length of
// the disasm string and the instruction length in bytes
void cpu_disasm_code(addr_t addr, char *buf, size_t buf_size,
                     size_t *str_used_len, size_t *inst_byte_len) {
  if (!buf || buf_size == 0) {
    if (str_used_len)
      *str_used_len = 0;
    if (inst_byte_len)
      *inst_byte_len = 0;
    return;
  }

  byte_t opcode = bus_read(cpu.bus, addr);
  struct inst i = inst_lookup[opcode];
  byte_t b1 = bus_read(cpu.bus, (addr_t)(addr + 1));
  byte_t b2 = bus_read(cpu.bus, (addr_t)(addr + 2));

  char operand[32] = {0};
  size_t len = 1;

  if (i.addr_mode == NULL) {
    snprintf(operand, sizeof(operand), "?");
  } else if (i.addr_mode == IMP) {
    operand[0] = '\0';
    len = 1;
  } else if (i.addr_mode == IMM) {
    snprintf(operand, sizeof(operand), "#%02X", b1);
    len = 2;
  } else if (i.addr_mode == ZP0) {
    snprintf(operand, sizeof(operand), "$%02X", b1);
    len = 2;
  } else if (i.addr_mode == ZPX) {
    snprintf(operand, sizeof(operand), "$%02X,X", b1);
    len = 2;
  } else if (i.addr_mode == ZPY) {
    snprintf(operand, sizeof(operand), "$%02X,Y", b1);
    len = 2;
  } else if (i.addr_mode == REL) {
    int8_t rel = (int8_t)b1;
    addr_t target = (addr_t)(addr + 2 + rel);
    snprintf(operand, sizeof(operand), "$%04X", target);
    len = 2;
  } else if (i.addr_mode == ABS) {
    addr_t abs = comb_addr(b2, b1);
    snprintf(operand, sizeof(operand), "$%04X", abs);
    len = 3;
  } else if (i.addr_mode == ABX) {
    addr_t abs = comb_addr(b2, b1);
    snprintf(operand, sizeof(operand), "$%04X,X", abs);
    len = 3;
  } else if (i.addr_mode == ABY) {
    addr_t abs = comb_addr(b2, b1);
    snprintf(operand, sizeof(operand), "$%04X,Y", abs);
    len = 3;
  } else if (i.addr_mode == IND) {
    addr_t abs = comb_addr(b2, b1);
    snprintf(operand, sizeof(operand), "($%04X)", abs);
    len = 3;
  } else if (i.addr_mode == IZX) {
    snprintf(operand, sizeof(operand), "($%02X,X)", b1);
    len = 2;
  } else if (i.addr_mode == IZY) {
    snprintf(operand, sizeof(operand), "($%02X),Y", b1);
    len = 2;
  } else {
    snprintf(operand, sizeof(operand), "?");
  }

  int written = 0;
  if (operand[0] == '\0')
    written = snprintf(buf, buf_size, "$%04X: %s", addr, i.op_name);
  else
    written = snprintf(buf, buf_size, "$%04X: %s %s", addr, i.op_name, operand);

  if (written < 0)
    written = 0;
  if (str_used_len)
    *str_used_len = (size_t)written;
  if (inst_byte_len)
    *inst_byte_len = len;
}