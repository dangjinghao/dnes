#include "dnes.h"
#include <assert.h>
#include <stdbool.h>
static struct {
  struct {
    byte_t C : 1;          // Carry
    byte_t Z : 1;          // Zero
    byte_t I : 1;          // Disable Interrupts
    byte_t D : 1;          // Decimal Mode(unused)
    byte_t B : 1;          // Break
    byte_t __unused__ : 1; // Unused
    byte_t V : 1;          // Overflow
    byte_t N : 1;          // Negative
  } status;
  byte_t A;
  byte_t X;
  byte_t Y;
  byte_t STKP;
  addr_t PC;
} cpu;

static addr_t addr_abs;
static addr_t addr_rel;
static byte_t opcode;
// current inst remaining cycles
static byte_t cycles;
static byte_t fetched;

static byte_t fetch();

/// Addressing Modes
// the return value means 'whether this addressing mode will increase the cycle'

#define comb_addr(hi, lo) ((addr_t)(((addr_t)(hi) << 8) | (addr_t)(lo)))
#define is_byte_neg(B) (((B) & 0x80) != 0)
#define addr_page(a) ((a) >> 8)
#define gen_flag(f) (!!(f))
#define stack_base (0x0100)
#define cpu_status_byte (*(byte_t *)&cpu.status)

static_assert(sizeof(cpu.status) == 1, "Unexcepted cpu status length");

static bool IMP() {
  // looks like it combined IMP and Accum addressing modes.
  fetched = cpu.A;
  return false;
}
static bool IMM() {
  addr_abs = cpu.PC++;
  return false;
}
// the data can be found in page 0
static bool ZP0() {
  addr_abs = bus_read(cpu.PC++);
  addr_abs &= 0x00FF;
  return false;
}
static bool ZPX() {
  addr_abs = (bus_read(cpu.PC++) + cpu.X);
  addr_abs &= 0x00FF;
  return false;
}
static bool ZPY() {
  addr_abs = (bus_read(cpu.PC++) + cpu.Y);
  addr_abs &= 0x00FF;
  return false;
}
static bool REL() {
  addr_rel = bus_read(cpu.PC++);
  // range: -128 ~ 127
  // if the readed rel address byte is negative,
  // we need to extend the signal bit
  if (is_byte_neg(addr_rel))
    addr_rel |= 0xFF00;
  return false;
}
static bool ABS() {
  byte_t lo = bus_read(cpu.PC++);
  byte_t hi = bus_read(cpu.PC++);

  addr_abs = comb_addr(hi, lo);

  return false;
}
static bool ABX() {
  byte_t lo = bus_read(cpu.PC++);
  byte_t hi = bus_read(cpu.PC++);

  addr_abs = comb_addr(hi, lo);
  addr_abs += cpu.X;

  if (addr_page(addr_abs) != hi) {
    // cross the page
    return true;
  }

  return false;
}
static bool ABY() {
  byte_t lo = bus_read(cpu.PC++);
  byte_t hi = bus_read(cpu.PC++);

  addr_abs = comb_addr(hi, lo);
  addr_abs += cpu.Y;

  if (addr_page(addr_abs) != hi) {
    // cross the page
    return true;
  }

  return false;
}
// indirect addressing
static bool IND() {

  byte_t lo = bus_read(cpu.PC++);
  byte_t hi = bus_read(cpu.PC++);
  addr_t addr = comb_addr(hi, lo);

  if (lo == 0xFF) {
    // 6502 hardware bug WARN: An indirect JMP (xxFF) will fail because the MSB
    // will be fetched from address xx00 instead of page xx+1.
    lo = bus_read(addr);
    // read xx00
    hi = bus_read(addr & 0xFF00);
  } else {
    lo = bus_read(addr);
    hi = bus_read(addr + 1);
  }
  addr_abs = comb_addr(hi, lo);

  return false;
}
// indirect address of 0 page with X register
static bool IZX() {
  addr_t t = bus_read(cpu.PC++);
  t = t + (addr_t)cpu.X;
  byte_t lo = bus_read(t & 0x00FF);
  byte_t hi = bus_read((t + 1) & 0x00FF);
  addr_abs = comb_addr(hi, lo);

  return false;
}
static bool IZY() {
  addr_t t = bus_read(cpu.PC++);
  byte_t lo = bus_read(t & 0x00FF);
  byte_t hi = bus_read((t + 1) & 0x00FF);

  addr_abs = comb_addr(hi, lo);
  addr_abs += cpu.Y;

  if (addr_page(addr_abs) != hi) {
    // cross the page
    return true;
  }

  return false;
}

/// Instructions
// the return means 'whether this instruction need the additional cycles'

// Addition with carry bit
static bool ADC() {
  assert(cpu.status.D == 0);

  // get the data
  fetch();

  uint16_t result =
      (uint16_t)cpu.A + (uint16_t)fetched + (uint16_t)cpu.status.C;
  cpu.status.C = gen_flag(result > 0x00FF);
  cpu.status.Z = gen_flag((result & 0x00FF) == 0);
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
  // SetFlag(V, (~((uint16_t)a ^ (uint16_t)fetched) &
  //            ((uint16_t)a ^ (uint16_t)temp)) & 0x0080);
  uint16_t tmp1 = ~((uint16_t)cpu.A ^ (uint16_t)fetched);
  uint16_t tmp2 = (uint16_t)cpu.A ^ (uint16_t)result;
  cpu.status.V = gen_flag((tmp1 & tmp2) & 0x0080);
  cpu.status.N = gen_flag(is_byte_neg((byte_t)result));
  cpu.A = result & 0x00FF;
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
  assert(cpu.status.D == 0);
  fetch();
  // ~M
  uint16_t M = ((uint16_t)fetched) ^ 0x00FF;
  uint16_t result = (uint16_t)cpu.A + (uint16_t)M + (uint16_t)cpu.status.C;
  cpu.status.C = gen_flag(result > 0x00FF);
  cpu.status.Z = gen_flag((result & 0x00FF) == 0);
  // Overflow: (+)A - (+)M = (-)
  //           (-)A - (-)M = (+)
  // SetFlag(V, (temp ^ (uint16_t)a) & (temp ^ value) & 0x0080);
  uint16_t tmp1 = (uint16_t)cpu.A ^ result;
  uint16_t tmp2 = (uint16_t)M ^ result;
  cpu.status.V = gen_flag((tmp1 & tmp2) & 0x0080);
  cpu.status.N = gen_flag(is_byte_neg((byte_t)result));
  cpu.A = result & 0x00FF;
  return true;
}

// bitwise logic and
static bool AND() {
  fetch();
  cpu.A &= fetched;
  cpu.status.Z = gen_flag(cpu.A == 0);
  cpu.status.N = gen_flag(is_byte_neg(cpu.A));
  return true;
}
static bool ASL();
// branch if Carry clear
static bool BCC() {
  if (!cpu.status.C) {
    cycles += 1;
    addr_abs = cpu.PC + addr_rel;
    // different page branch, +1 cycle
    if (addr_page(addr_abs) != addr_page(cpu.PC))
      cycles += 1;

    cpu.PC = addr_abs;
  }
  return false;
}
// branch if carry set
static bool BCS() {
  if (cpu.status.C) {
    cycles += 1;
    addr_abs = cpu.PC + addr_rel;
    // different page branch, +1 cycle
    if (addr_page(addr_abs) != addr_page(cpu.PC))
      cycles += 1;

    cpu.PC = addr_abs;
  }
  return false;
}
// branch if equal
static bool BEQ() {
  if (cpu.status.Z) {
    cycles += 1;
    addr_abs = cpu.PC + addr_rel;
    if (addr_page(addr_abs) != addr_page(cpu.PC))
      cycles += 1;

    cpu.PC = addr_abs;
  }

  return false;
}
static bool BIT();
// branch if negative
static bool BMI() {
  if (cpu.status.N) {
    cycles += 1;
    addr_abs = cpu.PC + addr_rel;

    if (addr_page(addr_abs) != addr_page(cpu.PC))
      cycles += 1;

    cpu.PC = addr_abs;
  }

  return false;
}
// branch if not equal
static bool BNE() {
  if (!cpu.status.Z) {
    cycles += 1;
    addr_abs = cpu.PC + addr_rel;

    if (addr_page(addr_abs) != addr_page(cpu.PC))
      cycles += 1;

    cpu.PC = addr_abs;
  }

  return false;
}
// branch if positive
static bool BPL() {
  if (!cpu.status.N) {
    cycles += 1;
    addr_abs = cpu.PC + addr_rel;

    if (addr_page(addr_abs) != addr_page(cpu.PC))
      cycles += 1;

    cpu.PC = addr_abs;
  }

  return false;
}
static bool BRK();
// branch if overflow clear
static bool BVC() {
  if (!cpu.status.V) {
    cycles += 1;
    addr_abs = cpu.PC + addr_rel;

    if (addr_page(addr_abs) != addr_page(cpu.PC))
      cycles += 1;

    cpu.PC = addr_abs;
  }

  return false;
}
// branch if overflow set
static bool BVS() {
  if (cpu.status.V) {
    cycles += 1;
    addr_abs = cpu.PC + addr_rel;

    if (addr_page(addr_abs) != addr_page(cpu.PC))
      cycles += 1;

    cpu.PC = addr_abs;
  }

  return false;
}
static bool CLC() {
  cpu.status.C = 0;
  return 0;
}
static bool CLD() {
  cpu.status.D = 0;
  return 0;
}
static bool CLI() {
  cpu.status.I = 0;
  return 0;
}
static bool CLV() {
  cpu.status.V = 0;
  return 0;
}
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
static bool PHA() {
  bus_write(stack_base + (cpu.STKP--), cpu.A);
  return false;
}
static bool PLA() {
  cpu.A = bus_read(stack_base + (++cpu.STKP));
  cpu.status.Z = gen_flag(cpu.A == 0x00);
  cpu.status.N = gen_flag(is_byte_neg(cpu.A));
  return false;
}
static bool PHP();
static bool PLP();
static bool ROL();
static bool ROR();
static bool RTI() {
  cpu_status_byte = bus_read(stack_base + (++cpu.STKP));
  cpu.status.B = 0;
  // WARN: __unused__ should be always 1, but olc6502 just reset it, that may be
  // wrong implementation.
  addr_t lo = (addr_t)bus_read(stack_base + (++cpu.STKP));
  addr_t hi = (addr_t)bus_read(stack_base + (++cpu.STKP));
  cpu.PC = comb_addr(hi, lo);
  return 0;
}
static bool RTS();
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
  char *name;
  bool (*operate)();
  bool (*addr_mode)();
  byte_t cycles;
};

#define GEN_INST(OP, AM, CY) {#OP, OP, AM, CY}
#define GEN_XXX {"???", XXX, IMP, 2}

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

void cpu_clock() {
  if (cycles == 0) {
    // previous inst is processed, fetch new one
    opcode = bus_read(cpu.PC++);
    struct inst i = inst_lookup[opcode];
    cycles = i.cycles;

    bool additional_cycle1 = i.addr_mode();
    bool additional_cycle2 = i.operate();
    if (additional_cycle1 && additional_cycle2)
      cycles += 1;
  }

  cycles -= 1;
}

static byte_t fetch() {
  if (inst_lookup[opcode].addr_mode != IMP)
    fetched = bus_read(addr_abs);

  return fetched;
}

void cpu_reset() {
  cpu.A = 0;
  cpu.X = 0;
  cpu.Y = 0;
  cpu.STKP = 0xFD;
  cpu_status_byte = 0;
  // this unused bit should always be 1 in the datasheet
  cpu.status.__unused__ = 1;
  // WARN: olc6502 doesn't set this
  cpu.status.I = 1;

  addr_abs = 0xFFFC;
  byte_t lo = bus_read(addr_abs);
  byte_t hi = bus_read(addr_abs + 1);
  // 7 cycles in std reset manual, they are previous operations.

  // 8th cycle, JMP to the address
  cpu.PC = comb_addr(hi, lo);

  addr_rel = 0x0000;
  addr_abs = 0x0000;
  fetched = 0x00;

  cycles = 8;
}
void cpu_irq() {
  if (cpu.status.I) {
    return;
  }
  bus_write(stack_base + (cpu.STKP--), addr_page(cpu.PC) & 0x00FF);
  bus_write(stack_base + (cpu.STKP--), cpu.PC & 0x00FF);

  cpu.status.B = 0;
  // bit 5 reads back as 1 on a 6502; keep stack copy consistent
  cpu.status.__unused__ = 1;
  // WARN: wrong implementation in olc6502, the I flag should be set after push
  bus_write(stack_base + (cpu.STKP--), cpu_status_byte);
  cpu.status.I = 1;
  addr_abs = 0xFFFE;
  byte_t lo = bus_read(addr_abs);
  byte_t hi = bus_read(addr_abs + 1);
  cpu.PC = comb_addr(hi, lo);
  cycles = 7;
}
void cpu_nmi() {
  bus_write(stack_base + (cpu.STKP--), addr_page(cpu.PC) & 0x00FF);
  bus_write(stack_base + (cpu.STKP--), cpu.PC & 0x00FF);
  cpu.status.B = 0;
  // bit 5 reads back as 1 on a 6502; keep stack copy consistent
  cpu.status.__unused__ = 1;
  // WARN: wrong implementation in olc6502, the I flag should be set after push
  bus_write(stack_base + (cpu.STKP--), cpu_status_byte);
  cpu.status.I = 1;
  addr_abs = 0xFFFA;
  byte_t lo = bus_read(addr_abs);
  byte_t hi = bus_read(addr_abs + 1);
  cpu.PC = comb_addr(hi, lo);
  cycles = 8;
}