#include "dnes.h"
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
//

#define comb_addr(hi, lo) ((addr_t)(((addr_t)hi << 8) | (addr_t)lo))

static bool IMP() {
  // looks like it combined IMP and Accum addressing modes.
  fetched = cpu.A;
  return false;
}
static bool IMM() {
  addr_abs = cpu.PC++;
  return false;
}
// the data can be found
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
  // if the readed rel byte is negative,
  // we need to extend the signal bit
  if (addr_rel & 0x80)
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

  if ((addr_abs >> 8) != hi) {
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

  if ((addr_abs >> 8) != hi) {
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
    // Nes bug WARN:  An indirect JMP (xxFF) will fail because the MSB will
    // be fetched from address xx00 instead of page xx+1.
    lo = bus_read(addr);
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

  if ((addr_abs >> 8) != hi) {
    // cross the page
    return true;
  }

  return false;
}
/// Opcodes
// 
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