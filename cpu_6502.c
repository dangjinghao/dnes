#include "dnes.h"

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
static byte_t cycles;

static byte_t fetched;
static byte_t fetch();
static void clock();
static void irq();
static void nmi();

// Addressing Modes
static byte_t IMP();
static byte_t IMM();
static byte_t ZP0();
static byte_t ZPX();
static byte_t ZPY();
static byte_t REL();
static byte_t ABS();
static byte_t ABX();
static byte_t ABY();
static byte_t IND();
static byte_t IZX();
static byte_t IZY();
// Opcodes
static byte_t ADC();
static byte_t AND();
static byte_t ASL();
static byte_t BCC();
static byte_t BCS();
static byte_t BEQ();
static byte_t BIT();
static byte_t BMI();
static byte_t BNE();
static byte_t BPL();
static byte_t BRK();
static byte_t BVC();
static byte_t BVS();
static byte_t CLC();
static byte_t CLD();
static byte_t CLI();
static byte_t CLV();
static byte_t CMP();
static byte_t CPX();
static byte_t CPY();
static byte_t DEC();
static byte_t DEX();
static byte_t DEY();
static byte_t EOR();
static byte_t INC();
static byte_t INX();
static byte_t INY();
static byte_t JMP();
static byte_t JSR();
static byte_t LDA();
static byte_t LDX();
static byte_t LDY();
static byte_t LSR();
static byte_t NOP();
static byte_t ORA();
static byte_t PHA();
static byte_t PHP();
static byte_t PLA();
static byte_t PLP();
static byte_t ROL();
static byte_t ROR();
static byte_t RTI();
static byte_t RTS();
static byte_t SBC();
static byte_t SEC();
static byte_t SED();
static byte_t SEI();
static byte_t STA();
static byte_t STX();
static byte_t STY();
static byte_t TAX();
static byte_t TAY();
static byte_t TSX();
static byte_t TXA();
static byte_t TXS();
static byte_t TYA();
// Invalid instruction trap
static byte_t XXX();

struct inst {
  char *name;
  byte_t (*operate)();
  byte_t (*addr_mode)();
  byte_t cycles;
};

#define GEN_INST(OP, AM, CY) {#OP, OP, AM, CY}
#define GEN_XXX {"???", XXX, NOP, 2}

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
