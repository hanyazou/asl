/* codeh80.c */
/*****************************************************************************/
/* SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only                     */
/*                                                                           */
/* AS-Portierung                                                             */
/*                                                                           */
/* Codegenerator hanyazou H80                                                */
/*                                                                           */
/*****************************************************************************/

#include "stdinc.h"
#include <ctype.h>
#include <string.h>

#include "nls.h"
#include "strutil.h"
#include "bpemu.h"
#include "asmdef.h"
#include "asmsub.h"
#include "asmpars.h"
#include "asmcode.h"
#include "asmallg.h"
#include "nlmessages.h"
#include "as.rsc"
#include "onoff_common.h"
#include "asmitree.h"
#include "codepseudo.h"
#include "intpseudo.h"
#include "codevars.h"
#include "cpu2phys.h"
#include "function.h"
#include "onoff_common.h"
#include "errmsg.h"

#include "codeh80.h"

// #define DEBUG 1

typedef enum
{
  e_core_h80,
} cpu_core_t;

typedef enum {
  bus_cmd_write =   0,
  bus_cmd_read =    1,
  bus_cmd_write_w = 2,
  bus_cmd_read_w =  3,
  bus_cmd_write_b = 4,
  bus_cmd_read_b =  5,
} bus_cmd_t;

typedef enum
{
  e_core_mask_h80 = 1 << e_core_h80,
  e_core_mask_all = e_core_mask_h80,
} cpu_core_mask_t;

typedef enum
{
  e_core_flag_none = 0,
  e_core_flag_16bit = 1 << 0,
} cpu_core_flags_t;

typedef struct
{
  const char *p_name;
  cpu_core_t core;
  cpu_core_flags_t core_flags;
} cpu_props_t;

#ifdef __cplusplus
# include "codez80.hpp"
#endif

#define ModNone (-1)
#define ModReg 1
#define ModIndReg 2
#define ModImm 3
#define ModAbs 4
#define ModIndAbs 5

#define MModReg (1 << ModReg)
#define MModIndReg (1 << ModIndReg)
#define MModImm (1 << ModImm)
#define MModAbs (1 << ModAbs)
#define MModIndAbs (1 << ModIndAbs)

/*-------------------------------------------------------------------------*/
/* Instruktionsgruppendefinitionen */

typedef struct {
  cpu_core_mask_t core_mask;
  Word Code;
} BaseOrder;

typedef struct {
  const char *Name;
  Byte Code;
} Condition;

/*-------------------------------------------------------------------------*/
/* debug */

#if !defined(DEBUG)
#define DEBUG 0
#endif

#define dprint(fmt, ...) \
  do { \
    if (DEBUG) { \
      printf("%14s(%4d): " fmt,  __func__, __LINE__, ##__VA_ARGS__); \
    } \
  } while (0)

/*-------------------------------------------------------------------------*/

static Byte AdrPart;
static tSymbolSize OpSize;
static LongWord AdrVal;
static Byte AdrVals[4];
static ShortInt AdrMode;

static BaseOrder *FixedOrders;
static Condition *Conditions;

static const cpu_props_t *p_curr_cpu_props;

typedef Word ins_t;
typedef Byte flag_num_t;
typedef Byte reg_num_t;
typedef LongWord bus_addr_t;

// static const int reg_flag = 16;
static const int     reg_flag_zero      = 0;  // equal zero
static const int     reg_flag_carry     = 1;  // carry / borrow
static const int     reg_flag_parity    = 2;  // 1: even / 0: odd
static const int     reg_flag_overflow  = 2;  // overflow flag and parity flag share the same field
static const int     reg_flag_sign      = 3;  // 1: negitive / 0: positive
static const int     reg_flag_none      = 0x7f;
static const int     reg_flag_not       = 0x80;
// static const int reg_pc = 17;
// static const int reg_sp = 18;
// static const int reg_bp = 19;

static const int bus_num_mem = 0;
static const int bus_num_io = 1;

typedef struct {
    char *name;
    int number;
} name_table_t;
static name_table_t RegNames[] = {
    { "r0", 0 },
    { "r1", 1 },
    { "r2", 2 },
    { "r3", 3 },
    { "r4", 4 },
    { "r5", 5 },
    { "r6", 6 },
    { "r7", 7 },
    { "r8", 8 },
    { "r9", 9 },
    { "r10", 10 },
    { "r11", 11 },
    { "r12", 12 },
    { "r13", 13 },
    { "r14", 14 },
    { "r15", 15 },
    { "r16", 16 },
    { "r17", 17 },
    { "r18", 18 },
    { "r19", 19 },
    { "r20", 20 },
    { "r21", 21 },
    { "r22", 22 },
    { "r23", 23 },
    { "r24", 24 },
    { "r25", 25 },
    { "r26", 26 },
    { "r27", 27 },
    { "r28", 28 },
    { "r29", 29 },
    { "r30", 30 },
    { "r31", 31 },

    { "fl", 16 },
    { "pc", 17 },
    { "sp", 18 },
    { "bp", 19 },
    { NULL },
};

static ins_t I_NOP()    { return 0x0000; }
static ins_t I_HALT()   { return 0x0001; }
static ins_t I_RET()    { return 0x0002; }
// 0000_0000_0000_0011 to 0111_1110 reserved
static ins_t I_INV()    { return 0x007f; }

static ins_t I_RET_N_(flag_num_t f) { return 0x0080 | f; }
// static ins_t I_RET_NZ()             { return I_RET_N_(reg_flag_zero); }
static ins_t I_RET_(flag_num_t f)   { return 0x0084 | f; }
// static ins_t I_RET_Z()              { return I_RET_(reg_flag_zero); }
// 0000_0000_1000_1000 to 1110_1111 reserved
static ins_t I_LD_R_I(reg_num_t r)  { return 0x00f0 | r; }

static ins_t I_PUSH_R(reg_num_t r)  { return 0x0100 | r; }
static ins_t I_POP_R(reg_num_t r)   { return 0x0110 | r; }
static ins_t I_EXTN_RW(reg_num_t r) { return 0x0120 | r; }
static ins_t I_EXTN_RB(reg_num_t r) { return 0x0130 | r; }

static ins_t I_CPL_R(reg_num_t r)   { return 0x0140 | r; }
static ins_t I_NEG_R(reg_num_t r)   { return 0x0150 | r; }
static ins_t I_LD_RW_I(reg_num_t r) { return 0x0160 | r; }
static ins_t I_LD_RW_SI(reg_num_t r){ return 0x0170 | r; }

static ins_t I_INVF(flag_num_t f)   { return 0x0180 | f; }
static ins_t I_SETF(flag_num_t f)   { return 0x0190 | f; }
static ins_t I_CLRF(flag_num_t f)   { return 0x01a0 | f; }
static ins_t I_TSTF(flag_num_t f)   { return 0x01b0 | f; }

static ins_t I_CALL_R(reg_num_t r)  { return 0x01c0 | r; }
static ins_t I_RST_N(bus_addr_t n)  { return 0x01d0 | (n/8); }
static ins_t I_JP_R(reg_num_t r)    { return 0x01e0 | r; }
static ins_t I_JR_R(reg_num_t r)    { return 0x01f0 | r; }

//  0 0010_00ff_rrrr CALLN f, (R) (call R if F is false)
static ins_t I_CALL_N_(flag_num_t f, reg_num_t r) {
  return 0x0200 | (f << 4) | r;
}
// static ins_t I_CALL_NZ(reg_num_t r)	{ return I_CALL_N_(reg_flag_zero, r); }

//  0 0010_01ff_rrrr CALL f, (R) (call to R if F is false)
static ins_t I_CALL_(flag_num_t f, reg_num_t r) {
  return 0x0240 | (f<<4) | r;
}
// static ins_t I_CALL_Z(reg_num_t r)  { return I_CALL_(reg_flag_zero, r); }

//  0 0010_10ff_rrrr reserved
//  0 0010_11ff_rrrr reserved

//  0 0011_00ff_rrrr JPN f, (R) (jump to R if F is false)
static ins_t I_JP_N_(flag_num_t f, reg_num_t r) {
  return 0x0300 | (f<<4) | r;
}
// static ins_t I_JP_NZ(reg_num_t r)   { return I_JP_N_(reg_flag_zero, r); }
// static ins_t I_JP_NC(reg_num_t r)   { return I_JP_N_(reg_flag_carry, r); }
// static ins_t I_JP_NV(reg_num_t r)   { return I_JP_N_(reg_flag_overflow, r); }
// static ins_t I_JP_NP(reg_num_t r)   { return I_JP_N_(reg_flag_parity, r); }
// static ins_t I_JP_NS(reg_num_t r)   { return I_JP_N_(reg_flag_sign, r); }

//  0 0011_01ff_rrrr JP f, (R) (jump to R if F is false)
static ins_t I_JP_(flag_num_t f, reg_num_t r) {
  return 0x0340 | (f<<4) | r;
}
// static ins_t I_JP_Z (reg_num_t r)   { return I_JP_(reg_flag_zero, r); }
// static ins_t I_JP_C (reg_num_t r)   { return I_JP_(reg_flag_carry, r); }
// static ins_t I_JP_V (reg_num_t r)   { return I_JP_(reg_flag_overflow, r); }
// static ins_t I_JP_P (reg_num_t r)   { return I_JP_(reg_flag_parity, r); }
// static ins_t I_JP_S (reg_num_t r)   { return I_JP_(reg_flag_sign, r); }

//  0 0011_10ff_rrrr JRN f, (R) (jump to R if F is false)
static ins_t I_JR_N_(flag_num_t f, reg_num_t r) {
  return 0x0380 | (f<<4) | r;
}
// static ins_t I_JR_NZ(reg_num_t r)   { return I_JR_N_(reg_flag_zero, r); }
// static ins_t I_JR_NC(reg_num_t r)   { return I_JR_N_(reg_flag_carry, r); }
// static ins_t I_JR_NV(reg_num_t r)   { return I_JR_N_(reg_flag_overflow, r); }
// static ins_t I_JR_NP(reg_num_t r)   { return I_JR_N_(reg_flag_parity, r); }
// static ins_t I_JR_NS(reg_num_t r)   { return I_JR_N_(reg_flag_sign, r); }

//  0 0011_11ff_rrrr JR f, (R) (jump to R if F is false)
static ins_t I_JR_(flag_num_t f, reg_num_t r) {
  return 0x03c0 | (f<<4) | r;
}
// static ins_t I_JR_Z (reg_num_t r)   { return I_JR_(reg_flag_zero, r); }
// static ins_t I_JR_C (reg_num_t r)   { return I_JR_(reg_flag_carry, r); }
// static ins_t I_JR_V (reg_num_t r)   { return I_JR_(reg_flag_overflow, r); }
// static ins_t I_JR_P (reg_num_t r)   { return I_JR_(reg_flag_parity, r); }
// static ins_t I_JR_S (reg_num_t r)   { return I_JR_(reg_flag_sign, r); }

static ins_t I_SRA_R_I(reg_num_t a, Byte n)  { return 0x0400 | (a<<4) | n; }
static ins_t I_SRL_R_I(reg_num_t a, Byte n)  { return 0x0500 | (a<<4) | n; }
static ins_t I_SL_R_I (reg_num_t a, Byte n)  { return 0x0600 | (a<<4) | n; }
static ins_t I_RLC_R_I(reg_num_t a, Byte n)  { return 0x0700 | (a<<4) | n; }
static ins_t I_ADD_R_I(reg_num_t a, Byte n)  { return 0x0800 | (a<<4) | n; }
static ins_t I_SUB_R_I(reg_num_t a, Byte n)  { return 0x0900 | (a<<4) | n; }

//  0 1010_aaaa_bbbb DJNZ A, (B) (decrement A and jump to B if A is not zero)
static ins_t I_DJNZ(reg_num_t a, reg_num_t b) { return 0x0a00 | (a<<4) | b; }

//  0 110a_aaaa_bbbb EX A, B
static ins_t I_EX_R_R(reg_num_t a, reg_num_t b) {
  if (((a>>4)&1) && ~((b>>4)&1))
    return 0x0c00 | (a << 4) | b;
  else
  if (~((a>>4)&1) && ((b>>4)&1))
    return 0x0c00 | (b << 4) | a;
  else
    return I_INV();
}

//  0 1110_aaaa_bbbb reserved 空き
//  0 1111_aaaa_bbbb reserved 空き

//  1 dddd_nnnn_nnnn  reg[D] = n
static ins_t I_LD_RB_I(reg_num_t r, int i) {
  return 0x1000 | (r<<8) | i;
}

//  2 dddd_nnnn_nnnn  reg[D] = n (signed)
static ins_t I_LD_RB_SI(reg_num_t r, int i) {
  return 0x2000 | (r<<8) | i;
}

//
//  memory load/store
//
//  3 ttt0_aaaa_abbb R/W reg[A] from/to memory address reg[B]
static ins_t I_BUS_ACCESS(Byte bus_cmd, Byte bus_num, reg_num_t ra, reg_num_t rb) {
  return 0x3000 | (bus_cmd<<9) | (bus_num<<8) | (ra<<4) | rb;
}
static ins_t I_LD_M_R (reg_num_t rb, reg_num_t ra) {
  return I_BUS_ACCESS(bus_cmd_write, bus_num_mem, ra, rb);
}
static ins_t I_LD_R_M (reg_num_t ra, reg_num_t rb) {
  return I_BUS_ACCESS(bus_cmd_read, bus_num_mem, ra, rb);
}
static ins_t I_LD_M_RW(reg_num_t rb, reg_num_t ra) {
  return I_BUS_ACCESS(bus_cmd_write_w, bus_num_mem, ra, rb);
}
static ins_t I_LD_RW_M(reg_num_t ra, reg_num_t rb) {
  return I_BUS_ACCESS(bus_cmd_read_w, bus_num_mem, ra, rb);
}
static ins_t I_LD_M_RB(reg_num_t rb, reg_num_t ra) {
  return I_BUS_ACCESS(bus_cmd_write_b, bus_num_mem, ra, rb);
}
static ins_t I_LD_RB_M(reg_num_t ra, reg_num_t rb) {
  return I_BUS_ACCESS(bus_cmd_read_b, bus_num_mem, ra, rb);
}

//
//  I/O read/write
//
//  3 ttt1_aaaa_abbb R/W reg[A] from/to I/O address reg[B]
/*
static ins_t I_OUT (reg_num_t rb, reg_num_t ra) {
  return I_BUS_ACCESS(bus_cmd_write, bus_num_io, ra, rb);
}
static ins_t I_IN  (reg_num_t ra, reg_num_t rb) {
  return I_BUS_ACCESS(bus_cmd_read, bus_num_io, ra, rb);
}
static ins_t I_OUTW(reg_num_t rb, reg_num_t ra) {
  return I_BUS_ACCESS(bus_cmd_write_w, bus_num_io, ra, rb);
}
static ins_t I_INW (reg_num_t ra, reg_num_t rb) {
  return I_BUS_ACCESS(bus_cmd_read_w, bus_num_io, ra, rb);
}
static ins_t I_OUTB(reg_num_t rb, reg_num_t ra) {
  return I_BUS_ACCESS(bus_cmd_write_b, bus_num_io, ra, rb);
}
static ins_t I_INB (reg_num_t ra, reg_num_t rb) {
  return I_BUS_ACCESS(bus_cmd_read_b, bus_num_io, ra, rb);
}
*/

//
//  move
//
static ins_t I_LD_R_R(reg_num_t rb, reg_num_t ra) {
  if ((((ra>>4)&1) && !((rb>>4)&1)) || (!((ra>>4)&1) && !((rb>>4)&1)))
    //  3 110a_aaaa_bbbb  move reg[A] to reg[B]
    return 0x3c00 | (ra<<4) | rb;
  else
  if (!((ra>>4)&1) && ((rb>>4)&1))
    //  3 111a_aaaa_bbbb  move reg[B] to reg[A]
    return 0x3e00 | (rb<<4) | ra;
  else
    return I_INV();
}

//
//  three register operations
//
static ins_t I_ADD(reg_num_t dst, reg_num_t ra, reg_num_t rb) {
  return 0x8000 | (dst<<8) | (ra<<4) | rb;
}
static ins_t I_SUB(reg_num_t dst, reg_num_t ra, reg_num_t rb) {
  return 0x9000 | (dst<<8) | (ra<<4) | rb;
}
static ins_t I_MUL(reg_num_t dst, reg_num_t ra, reg_num_t rb) {
  return 0xa000 | (dst<<8) | (ra<<4) | rb;
}
static ins_t I_DIV(reg_num_t dst, reg_num_t ra, reg_num_t rb) {
  return 0xb000 | (dst<<8) | (ra<<4) | rb;
}
static ins_t I_AND(reg_num_t dst, reg_num_t ra, reg_num_t rb) {
  return 0xc000 | (dst<<8) | (ra<<4) | rb;
}
static ins_t I_OR (reg_num_t dst, reg_num_t ra, reg_num_t rb) {
  return 0xd000 | (dst<<8) | (ra<<4) | rb;
}
static ins_t I_XOR(reg_num_t dst, reg_num_t ra, reg_num_t rb) {
  return 0xe000 | (dst<<8) | (ra<<4) | rb;
}
static ins_t I_CP (reg_num_t dst, reg_num_t ra, reg_num_t rb) {
  return 0xf000 | (dst<<8) | (ra<<4) | rb;
}

enum {
  G_NOP,
  G_HALT,
  G_RET,
  G_LD,
  G_LD_B,
  G_LD_W,
  G_LD_SB,
  G_LD_SW,
  G_PUSH,
  G_POP,
  G_EXTN_W,
  G_EXTN_B,
  G_CPL,
  G_NEG,
  G_INVF,
  G_SETF,
  G_CLRF,
  G_TSTF,
  G_CALL,
  G_RST,
  G_JP,
  G_JR,
  G_SRA,
  G_SRL,
  G_SL,
  G_RLC,
  G_DJNZ,
  G_EX,
  G_OUT,
  G_IN,
  G_ADD,
  G_SUB,
  G_MUL,
  G_DIV,
  G_AND,
  G_OR,
  G_XOR,
  G_CP,
};

/*!------------------------------------------------------------------------
 * \fn     EvalAbsAdrExpression(const tStrComp *pArg, tEvalResult *pEvalResult)
 * \brief  evaluate absolute address, range is targent-dependant
 * \param  pArg source argument
 * \param  pEvalResult sideband params
 * \return address value
 * ------------------------------------------------------------------------ */

static LongWord EvalAbsAdrExpression(const tStrComp *pArg, tEvalResult *pEvalResult)
{
  return EvalStrIntExpressionWithResult(pArg, Int32, pEvalResult);
}

/*==========================================================================*/
/* Adressparser */

/*!------------------------------------------------------------------------
 * \fn     DecodeReg(const tStrComp *p_arg, Byte *p_ret, tSymbolSize *p_size, tSymbolSize req_size, Boolean must_be_reg)
 * \brief  check whether argument is a CPU register or user-defined register alias
 * \param  p_arg argument
 * \param  p_value resulting register # if yes
 * \param  p_size resulting register size if yes
 * \param  req_size requested register size
 * \param  must_be_reg expecting register or maybe not?
 * \return reg eval result
 * ------------------------------------------------------------------------ */

static Boolean chk_reg_size(tSymbolSize req_size, tSymbolSize act_size)
{
  return (req_size == eSymbolSizeUnknown)
      || (req_size == act_size);
}

static tRegEvalResult DecodeReg(const tStrComp *p_arg, Byte *p_ret, tSymbolSize *p_size,
                                tSymbolSize req_size, Boolean must_be_reg)
{
  tRegEvalResult reg_eval_result;
  tEvalResult eval_result;
  tRegDescr reg_descr;

  name_table_t *p = RegNames;
  while (p->name != NULL && as_strcasecmp(p->name, p_arg->str.p_str) != 0) {
    p++;
  }
  if (p->name != NULL) {
    *p_ret = p->number;
    eval_result.DataSize = eSymbolSize32Bit;
    reg_eval_result = eIsReg;
  } else {
    reg_eval_result = EvalStrRegExpressionAsOperand(p_arg, &reg_descr, &eval_result,
                                                    eSymbolSizeUnknown, must_be_reg);
    if (reg_eval_result == eIsReg)
      *p_ret = reg_descr.Reg;
  }

  if (reg_eval_result == eIsReg) {
    if (!chk_reg_size(req_size, eval_result.DataSize)) {
      WrStrErrorPos(ErrNum_InvOpSize, p_arg);
      reg_eval_result = must_be_reg ? eIsNoReg : eRegAbort;
    }
  }

  if (p_size) *p_size = eval_result.DataSize;
  return reg_eval_result;
}

typedef struct
{
  as_eval_cb_data_t cb_data;
  Byte addr_reg;
  tSymbolSize addr_reg_size;
} z80_eval_cb_data_t;

DECLARE_AS_EVAL_CB(h80_eval_cb)
{
  z80_eval_cb_data_t *p_z80_eval_cb_data = (z80_eval_cb_data_t*)p_data;
  tSymbolSize this_reg_size;
  Byte this_reg;

  switch (DecodeReg(p_arg, &this_reg, &this_reg_size, eSymbolSizeUnknown, False)) {
    case eIsReg:
      if ((p_z80_eval_cb_data->addr_reg != 0xff)
       || !as_eval_cb_data_stack_plain_add(p_data->p_stack)) {
        WrStrErrorPos(ErrNum_InvAddrMode, p_arg);
        return e_eval_fail;
      }
      p_z80_eval_cb_data->addr_reg = this_reg;
      p_z80_eval_cb_data->addr_reg_size = this_reg_size;
      as_tempres_set_int(p_res, 0);
      return e_eval_ok;
    case eRegAbort:
      return e_eval_fail;
    default:
      return e_eval_none;
  }
}

static ShortInt DecodeAdr(const tStrComp *pArg, unsigned ModeMask)
{
  Boolean OK, is_indirect;
  tEvalResult EvalResult;

  AdrMode = ModNone;
  AdrCnt = 0;
  AdrPart = 0;

  /* 1. registers ? */

  switch (DecodeReg(pArg, &AdrPart, &EvalResult.DataSize, eSymbolSizeUnknown, False)) {
    case eRegAbort:
      goto found;
    case eIsReg:
      AdrMode = ModReg;
      goto found;
    default:
      break;
  }

  /* all types of indirect expressions (...): */

  is_indirect = IsIndirect(pArg->str.p_str);
  if (is_indirect) {
    tStrComp arg;
    tEvalResult disp_eval_result;
    z80_eval_cb_data_t z80_eval_cb_data;

    /* strip outer braces and spaces */

    StrCompRefRight(&arg, pArg, !!is_indirect);
    StrCompShorten(&arg, !!is_indirect);
    KillPrefBlanksStrCompRef(&arg);
    KillPostBlanksStrComp(&arg);

    /* walk through the components : */

    as_eval_cb_data_ini(&z80_eval_cb_data.cb_data, h80_eval_cb);
    z80_eval_cb_data.addr_reg = 0xff;
    z80_eval_cb_data.addr_reg_size = eSymbolSizeUnknown;
    EvalStrIntExprWithResultAndCallback(&arg, Int32, &disp_eval_result, &z80_eval_cb_data.cb_data);
    if (!disp_eval_result.OK)
      goto found;

    /* now we have parsed the expression, see what we can do with it: */

    if (z80_eval_cb_data.addr_reg != 0xff) {
      /* register indirect */
      AdrMode = ModIndReg;
      AdrPart = z80_eval_cb_data.addr_reg;
      goto found;
    }

    /* no register means absolute address indirect. */
    if (!(ModeMask & MModIndAbs)) {
      goto inv_mode;
    }

    switch (OpSize) {
      case eSymbolSize8Bit:
        AdrVal = EvalStrIntExpression(&arg, Int8, &OK);
        if (!OK)
          goto inv_mode;
        AdrMode = ModIndAbs;
        AdrVals[AdrCnt++] = AdrVal;
        break;
      case eSymbolSize16Bit:
        AdrVal = EvalStrIntExpression(&arg, Int16, &OK);
        if (!OK)
          goto inv_mode;
        AdrMode = ModIndAbs;
        AdrVals[AdrCnt++] = Lo(AdrVal);
        AdrVals[AdrCnt++] = Hi(AdrVal);
        break;
      case eSymbolSizeUnknown:
        AdrVal = EvalStrIntExpression(&arg, Int32, &OK);
        if (!OK)
          goto inv_mode;
        AdrMode = ModIndAbs;
        AdrVals[AdrCnt++] = (AdrVal >>  0) & 0xff;
        AdrVals[AdrCnt++] = (AdrVal >>  8) & 0xff;
        AdrVals[AdrCnt++] = (AdrVal >> 16) & 0xff;
        AdrVals[AdrCnt++] = (AdrVal >> 24) & 0xff;
        break;
      default:
        break;
    }
    goto found;
  }

  /* absolute address ? */

  if (ModeMask & MModAbs) {
    AdrVal = EvalAbsAdrExpression(pArg, &EvalResult);
    if (EvalResult.OK) {
      AdrMode = ModAbs;
      goto found;
    }
  }

  /* ...immediate */

  if (!(ModeMask & MModImm))
    goto inv_mode;

  switch (OpSize) {
    case eSymbolSize8Bit:
      AdrVal = EvalStrIntExpression(pArg, Int8, &OK);
      if (!OK)
        goto inv_mode;
      AdrMode = ModImm;
      AdrVals[AdrCnt++] = AdrVal;
      break;
    case eSymbolSize16Bit:
      AdrVal = EvalStrIntExpression(pArg, Int16, &OK);
      if (!OK)
        goto inv_mode;
      AdrMode = ModImm;
      AdrVals[AdrCnt++] = Lo(AdrVal);
      AdrVals[AdrCnt++] = Hi(AdrVal);
      break;
    case eSymbolSizeUnknown:
      AdrVal = EvalStrIntExpression(pArg, Int32, &OK);
      if (!OK)
        goto inv_mode;
      AdrMode = ModImm;
      AdrVals[AdrCnt++] = (AdrVal >>  0) & 0xff;
      AdrVals[AdrCnt++] = (AdrVal >>  8) & 0xff;
      AdrVals[AdrCnt++] = (AdrVal >> 16) & 0xff;
      AdrVals[AdrCnt++] = (AdrVal >> 24) & 0xff;
      break;
    default:
      break;
  }

found:
  if ((AdrMode != ModNone) && !(ModeMask & (1 << AdrMode)))
    goto inv_mode;
  return AdrMode;

inv_mode:
  WrStrErrorPos(ErrNum_InvAddrMode, pArg);
  AdrMode = ModNone;
  return AdrMode;
}

static void AppendVals(const Byte *pVals, unsigned ValLen)
{
  memcpy(BAsmCode + CodeLen, pVals, ValLen);
  CodeLen += ValLen;
}

static void AppendAdrVals(void)
{
  AppendVals(AdrVals, AdrCnt);
}

static void __AppendIns(const ins_t ins)
{
  BAsmCode[CodeLen++] = Lo(ins);
  BAsmCode[CodeLen++] = Hi(ins);
}

#define STRINGIFY(x) #x
#define TOSTRING(x) STRINGIFY(x)
#define AppendIns(...) do { \
    __AppendIns(__VA_ARGS__); \
    dprint("%s\n", TOSTRING(__VA_ARGS__));  \
  } while (0)

/*-------------------------------------------------------------------------*/
/* Bedingung entschluesseln */

static Boolean DecodeCondition(const char *Name, int *Erg)
{
  int z;

  for (z = 0; Conditions[z].Name; z++) {
    if (!as_strcasecmp(Conditions[z].Name, Name)) {
      *Erg = Conditions[z].Code;
      return True;
    }
  }
  *Erg = 0;
  return False;
}

/*==========================================================================*/
/* instruction decoders */

/*!------------------------------------------------------------------------
 * \fn     Boolean chk_core_mask(cpu_core_mask_t core_mask)
 * \brief  check whether current core fulfills requirement
 * \param  core_mask bit mask of supported cores
 * \return True if yes
 * ------------------------------------------------------------------------ */

static Boolean chk_core_mask(cpu_core_mask_t core_mask)
{
  if (!((core_mask >> p_curr_cpu_props->core) & 1)) {
    WrStrErrorPos(ErrNum_InstructionNotSupported, &OpPart);
    return False;
  }
  return True;
}

/*!------------------------------------------------------------------------
 * \fn     DecodeFixed(Word Index)
 * \brief  handle instructions without arguments
 * \param  Index * to instruction description
 * ------------------------------------------------------------------------ */

static void DecodeFixed(Word Index)
{
  BaseOrder *POrder = FixedOrders + Index;

  if (ChkArgCnt(0, 0) && chk_core_mask(POrder->core_mask)) {
    BAsmCode[CodeLen++] = Lo(POrder->Code);
    BAsmCode[CodeLen++] = Hi(POrder->Code);
  } else {
    // TODO, output error here
  }
}

static void LoadImm(reg_num_t r, LongWord imm)
{
    if ((imm & 0xffffff80) == 0xffffff80) {
      AppendIns(I_LD_RB_SI(r, imm & 0xff));
    } else
    if ((imm & 0xffff8000) == 0xffff8000) {
      AppendIns(I_LD_RW_SI(r));
      AppendIns(imm & 0xffff);
    } else
    if ((imm & 0xffffff00) == 0x00000000) {
      AppendIns(I_LD_RB_I(r, imm & 0xff));
    } else
    if ((imm & 0xffff0000) == 0x00000000) {
      AppendIns(I_LD_RW_I(r));
      AppendIns(imm & 0xffff);
    } else {
      AppendIns(I_LD_R_I(r));
      BAsmCode[CodeLen++] = ((imm >>  0) & 0xff);
      BAsmCode[CodeLen++] = ((imm >>  8) & 0xff);
      BAsmCode[CodeLen++] = ((imm >> 16) & 0xff);
      BAsmCode[CodeLen++] = ((imm >> 24) & 0xff);
    }
}

/*!------------------------------------------------------------------------
 * \fn     DecodeLD(size)
 * \brief  handle LD instruction
 * \param  LD, LD.B or LD.W
 * ------------------------------------------------------------------------ */

static void DecodeLD(Word size)
{
  reg_num_t dst, src;
  Boolean extend_sign;
  tSymbolSize op_size;
  LongInt dst_adr;

  if (!ChkArgCnt(2, 2))
      return;

  switch (size) {
  case G_LD:    op_size = eSymbolSizeUnknown; extend_sign = False; break;
  case G_LD_B:  op_size = eSymbolSize8Bit;    extend_sign = False; break;
  case G_LD_W:  op_size = eSymbolSize16Bit;   extend_sign = False; break;
  case G_LD_SB: op_size = eSymbolSize8Bit;    extend_sign = True; break;
  case G_LD_SW: op_size = eSymbolSize16Bit;   extend_sign = True; break;
  default:
    WrError(ErrNum_InternalError);
    return;
  }

  OpSize = eSymbolSizeUnknown;
  DecodeAdr(&ArgStr[1], MModReg | MModIndReg | MModIndAbs);
  dst = AdrPart;
  switch (AdrMode) {
  case ModReg:
    OpSize = eSymbolSizeUnknown;
    DecodeAdr(&ArgStr[2], MModReg | MModIndReg | MModImm | MModIndAbs);
    switch (AdrMode) {
    case ModReg:    /* LD R, R */
      src = AdrPart;
      if (((dst>>4)&1) && ((src>>4)&1)) {
        WrError(ErrNum_InvAddrMode);
        return;
      }
      AppendIns(I_LD_R_R(dst, src));
      break;
    case ModIndReg: /* LD R, (R) */
      src = AdrPart;
      if (((dst>>4)&1) || ((src>>4)&1)) {
        WrError(ErrNum_InvAddrMode);
        return;
      }
      switch (op_size) {
      case eSymbolSizeUnknown:  AppendIns(I_LD_R_M(dst, src));  break;
      case eSymbolSize8Bit:     AppendIns(I_LD_RB_M(dst, src)); break;
      case eSymbolSize16Bit:    AppendIns(I_LD_RW_M(dst, src)); break;
      default: WrError(ErrNum_InternalError); return;
      }
      break;
    case ModImm:    /* LD R, imm */
      if ((dst>>4)&1) {
        WrError(ErrNum_InvAddrMode);
        return;
      }
      OpSize = op_size;
      DecodeAdr(&ArgStr[2], MModImm);
      switch (op_size) {
      case eSymbolSizeUnknown:
        LoadImm(dst, AdrVal);
        return;
      case eSymbolSize8Bit:
        if (extend_sign)
          AppendIns(I_LD_RB_SI(dst, AdrVals[0]));
        else
          AppendIns(I_LD_RB_I(dst, AdrVals[0]));
        AdrCnt = 0;
        break;
      case eSymbolSize16Bit:
        if (extend_sign)
          AppendIns(I_LD_RW_SI(dst));
        else
          AppendIns(I_LD_RW_I(dst));
        break;
      default: WrError(ErrNum_InternalError); return;
      }
      AppendAdrVals();
      break;
    case ModIndAbs: /* LD R, (imm) */
      src = 0;
      LoadImm(src, AdrVal);
      switch (op_size) {
      case eSymbolSizeUnknown:  AppendIns(I_LD_R_M(dst, src));  break;
      case eSymbolSize8Bit:     AppendIns(I_LD_RB_M(dst, src)); break;
      case eSymbolSize16Bit:    AppendIns(I_LD_RW_M(dst, src)); break;
      default: WrError(ErrNum_InternalError); return;
      }
      break;
    default:
      if (AdrMode != ModNone) {
        WrError(ErrNum_InvAddrMode);
      }
      break;
    }
    break;
  case ModIndReg:
    DecodeAdr(&ArgStr[2], MModReg | MModImm);
    switch (AdrMode) {
    case ModReg:    /* LD (R), R */
      src = AdrPart;
      switch (op_size) {
      case eSymbolSizeUnknown:  AppendIns(I_LD_M_R(dst, src));  break;
      case eSymbolSize8Bit:     AppendIns(I_LD_M_RB(dst, src)); break;
      case eSymbolSize16Bit:    AppendIns(I_LD_M_RW(dst, src)); break;
      default: WrError(ErrNum_InternalError); return;
      }
      break;
    case ModImm:    /* LD (R), imm */
      src = 0;
      LoadImm(src, AdrVal);
      switch (op_size) {
      case eSymbolSizeUnknown:  AppendIns(I_LD_M_R(dst, src));  break;
      case eSymbolSize8Bit:     AppendIns(I_LD_M_RB(dst, src)); break;
      case eSymbolSize16Bit:    AppendIns(I_LD_M_RW(dst, src)); break;
      default: WrError(ErrNum_InternalError); return;
      }
      break;
    default:
      if (AdrMode != ModNone) {
        WrError(ErrNum_InvAddrMode);
      }
      return;
    }
    break;
  case ModIndAbs:
    dst_adr = AdrVal;
    DecodeAdr(&ArgStr[2], MModReg);
    switch (AdrMode) {
    case ModReg:    /* LD (imm), R */
      src = AdrPart;
      dst = 0;
      LoadImm(dst, dst_adr);
      switch (op_size) {
      case eSymbolSizeUnknown:  AppendIns(I_LD_M_R(dst, src));  break;
      case eSymbolSize8Bit:     AppendIns(I_LD_M_RB(dst, src)); break;
      case eSymbolSize16Bit:    AppendIns(I_LD_M_RW(dst, src)); break;
      default: WrError(ErrNum_InternalError); return;
      }
      break;
    default:
      if (AdrMode != ModNone) {
        WrError(ErrNum_InvAddrMode);
      }
      return;
    }
    break;
  default:
    if (AdrMode != ModNone) {
      WrError(ErrNum_InvAddrMode);
    }
    return;
  }
}

static void DecodeImm1(Word Code)
{
  Byte imm;

  if (!ChkArgCnt(1, 1))
      return;

  DecodeAdr(&ArgStr[1], MModImm);
  if (AdrMode != ModImm) return;
  imm = AdrVals[0];

  switch (Code) {
  case G_INVF:
  case G_SETF:
  case G_CLRF:
  case G_TSTF:
    if (imm & ~0xf) {
      WrError(ErrNum_ArgOutOfRange);
      return;
    }
    break;
  default:
    WrError(ErrNum_InternalError);
    return;
  }

  switch (Code) {
  case G_INVF:  AppendIns(I_INVF(imm)); break;
  case G_SETF:  AppendIns(I_SETF(imm)); break;
  case G_CLRF:  AppendIns(I_CLRF(imm)); break;
  case G_TSTF:  AppendIns(I_TSTF(imm)); break;
  default: WrError(ErrNum_InternalError); return;
  }
}

static void DecodeReg1(Word Code)
{
  reg_num_t r;

  if (!ChkArgCnt(1, 1))
      return;

  DecodeAdr(&ArgStr[1], MModReg);
  if (AdrMode != ModReg) return;
  r = AdrPart;

  switch (Code) {
  case G_EXTN_W:
  case G_EXTN_B:
  case G_CPL:
  case G_NEG:
    if (15 < r) {
      WrError(ErrNum_ArgOutOfRange);
      return;
    }
    break;
  default:
    WrError(ErrNum_InternalError);
    return;
  }

  switch (Code) {
  case G_EXTN_W:    AppendIns(I_EXTN_RW(r));    break;
  case G_EXTN_B:    AppendIns(I_EXTN_RB(r));    break;
  case G_CPL:       AppendIns(I_CPL_R(r));      break;
  case G_NEG:       AppendIns(I_NEG_R(r));      break;
  default: WrError(ErrNum_InternalError); return;
  }
}

static void DecodeReg2(Word Code)
{
  reg_num_t ra, rb;

  if (!ChkArgCnt(2, 2))
      return;

  DecodeAdr(&ArgStr[1], MModReg);
  if (AdrMode != ModReg) return;
  ra = AdrPart;

  DecodeAdr(&ArgStr[2], MModReg);
  if (AdrMode != ModReg) return;
  rb = AdrPart;

  switch (Code) {
  case G_EX:
    if (((ra>>4)&1) && ((rb>>4)&1)) {
      WrError(ErrNum_InvReg);
      return;
    }
    AppendIns(I_EX_R_R(ra, rb));
    break;
  default:
    WrError(ErrNum_InternalError);
    return;
  }
}

static void DecodeALU(Word Code)
{
  reg_num_t dst, ra, rb;
  Byte imm;

  OpSize = eSymbolSizeUnknown;
  if (ArgCnt == 2) {
    DecodeAdr(&ArgStr[1], MModReg | MModImm);
    if (AdrMode == ModImm && Code == G_CP) {
      LoadImm(1, AdrVal);
      dst = 1;
      ra = 1;
    } else {
      if (AdrMode == ModReg) {
        dst = AdrPart;
      } else {
        if (AdrMode != ModNone) {
          WrError(ErrNum_InvAddrMode);
        }
        return;
      }
    }

    DecodeAdr(&ArgStr[2], MModReg | MModImm);
    if (AdrMode == ModReg) {
      ra = dst;
      rb = AdrPart;
      goto three_registers;
    }
    if (AdrMode != ModImm) {
      if (AdrMode != ModNone) {
        WrError(ErrNum_InvAddrMode);
      }
      return;
    }
    imm = AdrVal;
    if ((imm & 0xfffffff0) == 0) {
      switch (Code) {
      case G_ADD: AppendIns(I_ADD_R_I(dst, imm)); return;
      case G_SUB: AppendIns(I_SUB_R_I(dst, imm)); return;
      }
    }

    LoadImm(0, AdrVal);
    ra = dst;
    rb = 0;
    goto three_registers;
  }

  if (!ChkArgCnt(3, 3))
      return;

  DecodeAdr(&ArgStr[1], MModReg);
  if (AdrMode != ModReg) return;
  dst = AdrPart;

  DecodeAdr(&ArgStr[2], MModReg | MModImm);
  if (AdrMode == ModImm) {
    LoadImm(0, AdrVal);
    ra = 0;
  } else {
    if (AdrMode == ModReg) {
      ra = AdrPart;
    } else {
      if (AdrMode != ModNone) {
        WrError(ErrNum_InvAddrMode);
      }
      return;
    }
  }

  DecodeAdr(&ArgStr[3], MModReg | MModImm);
  if (AdrMode == ModImm) {
    LoadImm(1, AdrVal);
    rb = 1;
  } else {
    if (AdrMode == ModReg) {
      rb = AdrPart;
    } else {
      if (AdrMode != ModNone) {
        WrError(ErrNum_InvAddrMode);
      }
      return;
    }
  }

 three_registers:
  switch (Code) {
  case G_ADD: AppendIns(I_ADD(dst, ra, rb));    break;
  case G_SUB: AppendIns(I_SUB(dst, ra, rb));    break;
  case G_MUL: AppendIns(I_MUL(dst, ra, rb));    break;
  case G_DIV: AppendIns(I_DIV(dst, ra, rb));    break;
  case G_AND: AppendIns(I_AND(dst, ra, rb));    break;
  case G_OR:  AppendIns(I_OR (dst, ra, rb));    break;
  case G_XOR: AppendIns(I_XOR(dst, ra, rb));    break;
  case G_CP:  AppendIns(I_CP (dst, ra, rb));    break;
  default: WrError(ErrNum_InternalError); return;
  }
}

static void DecodeShift(Word Code)
{
  reg_num_t reg_num = 0;
  Byte imm;

  if (!ChkArgCnt(2, 2))
    return;

  OpSize = eSymbolSizeUnknown;
  DecodeAdr(&ArgStr[1], MModReg);
  if (AdrMode != ModReg) return;
  reg_num = AdrPart;

  OpSize = eSymbolSize8Bit;
  DecodeAdr(&ArgStr[2], MModImm);
  if (AdrMode != ModImm) return;
  imm = AdrVals[0];

  switch (Code) {
  case G_SRA: AppendIns(I_SRA_R_I(reg_num, imm)); break;
  case G_SRL: AppendIns(I_SRL_R_I(reg_num, imm)); break;
  case G_SL:  AppendIns(I_SL_R_I (reg_num, imm)); break;
  case G_RLC: AppendIns(I_RLC_R_I(reg_num, imm)); break;
  default: WrError(ErrNum_InternalError); return;
  }
}

/*!------------------------------------------------------------------------
 * \fn     DecodePUSH_POP(Word Code)
 * \brief  handle PUSH/POP instructions
 * \param  Code 0 = PUSH, 1 = POP
 * ------------------------------------------------------------------------ */

static void DecodePUSH_POP(Word Code)
{
  reg_num_t reg_num = 0;

  if (!ChkArgCnt(1, 1))
    return;

  DecodeAdr(&ArgStr[1], MModReg);
  if (AdrMode != ModReg) {
    if (AdrMode != ModNone) {
      WrError(ErrNum_InvAddrMode);
    }
    return;
  }
  reg_num = AdrPart;

  if (Code == G_PUSH) {
    AppendIns(I_PUSH_R(reg_num));
  } else
  if (Code == G_POP) {
    AppendIns(I_POP_R(reg_num));
  } else {
    WrError(ErrNum_InternalError);
  }
}

static void DecodeIN_OUT(Word bus_cmd)
{
  reg_num_t port_reg_num;
  reg_num_t reg_num;
  LongInt rewind_len = CodeLen;

  if (!ChkArgCnt(2, 2))
    return;

  Boolean in = (bus_cmd & 0x1) ? True : False;
  const tStrComp *pPortArg = (bus_cmd & 0x1) ? &ArgStr[2] : &ArgStr[1];
  const tStrComp *pRegArg =  (bus_cmd & 0x1) ? &ArgStr[1] : &ArgStr[2];

  OpSize = eSymbolSizeUnknown;

  DecodeAdr(pPortArg, MModIndReg | MModIndAbs);
  if (AdrMode == ModIndReg) {
    port_reg_num = AdrPart;
  } else {
    if (AdrMode == ModIndAbs) {
      port_reg_num = 0;
      LoadImm(port_reg_num, AdrVal);
    } else {
      if (AdrMode != ModNone) {
        WrError(ErrNum_InvAddrMode);
      }
      CodeLen = rewind_len;
      return;
    }
  }

  DecodeAdr(pRegArg, in ? MModReg : (MModReg | MModImm));
  if (AdrMode == ModReg) {
    reg_num = AdrPart;
  } else {
    if (!in && AdrMode == ModImm) {
      reg_num = 1;
      LoadImm(reg_num, AdrVal);
    } else {
      if (AdrMode != ModNone) {
        WrError(ErrNum_InvAddrMode);
      }
      CodeLen = rewind_len;
      return;
    }
  }

  AppendIns(I_BUS_ACCESS(bus_cmd, bus_num_io, reg_num, port_reg_num));
}

static void DecodeRET(Word Code)
{
  int Cond;

  UNUSED(Code);

  if (!ChkArgCnt(0, 1))
    return;

  /*
   * unconditinal return
   */
  if (ArgCnt == 0) {
    AppendIns(I_RET());
    return;
  }

  /*
   * conditinal return
   */
  if (!DecodeCondition(ArgStr[1].str.p_str, &Cond)) {
    WrStrErrorPos(ErrNum_UndefCond, &ArgStr[1]);
    return;
  }

  if (Cond & reg_flag_not) {
    AppendIns(I_RET_N_(Cond & ~reg_flag_not));
  } else {
    AppendIns(I_RET_(Cond));
  }
}

static void encode_jump_reg(Word Code, int Cond, reg_num_t r, reg_num_t dec_r)
{
  switch (Code) {
  case G_JP:
    if (Cond == reg_flag_none) {
      AppendIns(I_JP_R(r));
    } else
    if (Cond & reg_flag_not) {
      AppendIns(I_JP_N_(Cond & ~reg_flag_not, r));
    } else {
      AppendIns(I_JP_(Cond, r));
    }
    break;
  case G_JR:
    if (Cond == reg_flag_none) {
      AppendIns(I_JR_R(r));
    } else
    if (Cond & reg_flag_not) {
      AppendIns(I_JR_N_(Cond & ~reg_flag_not, r));
    } else {
      AppendIns(I_JR_(Cond, r));
    }
    break;
  case G_CALL:
    if (Cond == reg_flag_none) {
      AppendIns(I_CALL_R(r));
    } else
    if (Cond & reg_flag_not) {
      AppendIns(I_CALL_N_(Cond & ~reg_flag_not, r));
    } else {
      AppendIns(I_CALL_(Cond, r));
    }
    break;
  case G_DJNZ:
    AppendIns(I_DJNZ(dec_r, r));
    break;
  default:
    WrError(ErrNum_InternalError);
    return;
  }
}

static void encode_jump_adr(Word Code, IntType dist_type, Byte condition,
                            LongInt dest, reg_num_t dec_r)
{
  switch (dist_type) {
  case SInt8:
  case SInt16:
    dprint("encode_jump_adr(Code=%d, dist=%d, Cond=0x%x, addr=%s%04x, dec_r=%d)\n",
           Code, dist_type, condition,
           (dest & (1 << 16)) ? "-" : "+",
           (dest & (1 << 16)) ? -dest : dest, dec_r);
    AppendIns(I_LD_RW_SI(0));  // load half word with sign extension
    BAsmCode[CodeLen++] = Lo(dest);
    BAsmCode[CodeLen++] = Hi(dest);
    break;
  case UInt8:
  case UInt16:
    dprint("encode_jump_adr(Code=%d, dist=%d, Cond=0x%x, addr=%04x, dec_r=%d)\n",
           Code, dist_type, condition, dest, dec_r);
    AppendIns(I_LD_RW_I(0));  // load half word (upper half of 32 bit will be cleared)
    BAsmCode[CodeLen++] = Lo(dest);
    BAsmCode[CodeLen++] = Hi(dest);
    break;
  case SInt24:
  case SInt32:
  case UInt24:
  case UInt32:
    dprint("encode_jump_adr(Code=%d, dist=%d, Cond=0x%x, addr=%08x, dec_r=%d)\n",
           Code, dist_type, condition, dest, dec_r);
    AppendIns(I_LD_R_I(0));
    BAsmCode[CodeLen++] = ((dest >>  0) & 0xff);
    BAsmCode[CodeLen++] = ((dest >>  8) & 0xff);
    BAsmCode[CodeLen++] = ((dest >> 16) & 0xff);
    BAsmCode[CodeLen++] = ((dest >> 24) & 0xff);
    break;
  default:
    WrError(ErrNum_InternalError);
    return;
  }

  encode_jump_reg(Code, condition, 0, dec_r);
}

static IntType abs_dest_type(LongWord dest)
{
  if (RangeCheck(dest, UInt16))
    return UInt16;
  return UInt32;
}

static IntType get_dist(Boolean relative, LongWord dest, LongInt *p_dest)
{
  LongWord orig = EProgCounter();
  *p_dest = dest - (orig + 4);  // 4 bytes (LD r0, nnnn)
  if (RangeCheck(*p_dest, SInt16))
    return SInt16;
  return SInt32;
}

static void DecodeJump(Word Code)
{
  reg_num_t dec_r = 0;
  reg_num_t r;
  int Cond;
  tEvalResult EvalResult;
  LongInt dest_adr;
  IntType abs_type, rel_type;
  Boolean prefer_short_adr;
  Boolean prefer_relative_adr;

  if (!ChkArgCnt(1, 2))
    return;

  switch (Code) {
  case G_DJNZ:
    prefer_short_adr = True;
    prefer_relative_adr = True;
    break;
  case G_JP:
    prefer_short_adr = False;
    prefer_relative_adr = False;
    break;
  case G_JR:
    prefer_short_adr = True;
    prefer_relative_adr = True;
    break;
  case G_CALL:
    prefer_short_adr = False;
    prefer_relative_adr = False;
    break;
  default:
    WrError(ErrNum_InternalError);
    return;
  }

  if (ArgCnt == 1) {
    switch (Code) {
    case G_DJNZ:
      (void)ChkArgCnt(2, 2);
      return;
    case G_JP:
    case G_JR:
    case G_CALL:
      Cond = reg_flag_none;
      break;
    }
  }
  if (ArgCnt == 2) {
    switch (Code) {
    case G_DJNZ:
      DecodeAdr(&ArgStr[1], MModReg);
      dec_r = AdrPart;
      if (AdrMode != ModReg) {
        if (AdrMode != ModNone) {
          WrError(ErrNum_InvAddrMode);
        }
        return;
      }
      break;
    case G_JP:
    case G_JR:
    case G_CALL:
      if (!DecodeCondition(ArgStr[1].str.p_str, &Cond)) {
        WrStrErrorPos(ErrNum_UndefCond, &ArgStr[1]);
        return;
      }
      break;
    }
  }

  OpSize = eSymbolSizeUnknown;
  DecodeAdr(&ArgStr[ArgCnt], MModIndReg | MModAbs);
  if (AdrMode != ModIndReg && AdrMode != ModAbs) {
    if (AdrMode != ModNone) {
      WrError(ErrNum_InvAddrMode);
    }
    return;
  }

  /*
   * jump to register indirect
   */
  if (AdrMode == ModIndReg) {
    r = AdrPart;
    encode_jump_reg(Code, Cond, r, dec_r);
    return;
  }

  /*
   * jump to specified address
   */
  AdrVal = EvalAbsAdrExpression(&ArgStr[ArgCnt], &EvalResult);
  if (!EvalResult.OK)
    return;

  abs_type = abs_dest_type(AdrVal);
  rel_type = get_dist(prefer_relative_adr, AdrVal, &dest_adr);
  if (Code == G_JP && abs_type == UInt32 && rel_type == SInt16) {
    prefer_short_adr = True;
    prefer_relative_adr = True;
    Code = G_JR;
  }
  if (Code == G_DJNZ && rel_type == SInt32) {
    prefer_short_adr = False;
    prefer_relative_adr = True;
  }

  if (prefer_short_adr && !mFirstPassUnknownOrQuestionable(EvalResult.Flags)
      && ((prefer_relative_adr && rel_type != SInt8 && rel_type != SInt16)
          || (!prefer_relative_adr && abs_type != UInt16 && abs_type != UInt16))) {
    WrStrErrorPos(ErrNum_JmpDistTooBig, &ArgStr[ArgCnt]);
    return;
  }

  if (prefer_relative_adr) {
    encode_jump_adr(Code, rel_type, Cond, dest_adr, dec_r);
  } else {
    encode_jump_adr(Code, abs_type, Cond, AdrVal, dec_r);
  }
}

static void DecodeRST(Word Code)
{
  UNUSED(Code);

  if (!ChkArgCnt(1, 1))
      return;

  Boolean OK;
  tSymbolFlags Flags;
  Byte AdrByte;
  int SaveRadixBase = RadixBase;

#if 0
  /* some people like to regard the RST argument as a literal
     and leave away the 'h' to mark 38 as a hex number... */
  RadixBase = 16;
#endif

  AdrByte = EvalStrIntExpressionWithFlags(&ArgStr[1], Int8, &OK, &Flags);
  RadixBase = SaveRadixBase;

  if (mFirstPassUnknown(Flags))
    AdrByte = AdrByte & 0x38;
  if (!OK)
    return;

  if ((AdrByte > 0x38) || (AdrByte & 7)) {
    WrError(ErrNum_NotFromThisAddress);
    return;
  }

  AppendIns(I_RST_N(AdrByte));
}

static void ModIntel(Word Code)
{
  UNUSED(Code);

  /* M80 compatibility: DEFB->DB, DEFW->DW and DEFM -> DB */
  if (as_strcasecmp(OpPart.str.p_str, "DEFM") == 0) {
    strmov(OpPart.str.p_str + 1, "B");
  } else {
    strmov(OpPart.str.p_str + 1, OpPart.str.p_str + 3);
  }
  DecodeIntelPseudo(False);
}

/*==========================================================================*/
/* Codetabellenerzeugung */

static void AddFixed(const char *NewName, cpu_core_mask_t core_mask, Word NewCode)
{
  order_array_rsv_end(FixedOrders, BaseOrder);
  FixedOrders[InstrZ].core_mask = core_mask;
  FixedOrders[InstrZ].Code = NewCode;
  dprint("%d: %s %02x %02x\n", InstrZ, NewName, (NewCode>>0)&0xff, (NewCode>>8)&0xff);
  AddInstTable(InstTable, NewName, InstrZ++, DecodeFixed);
}

static void AddCondition(const char *NewName, Byte NewCode)
{
  order_array_rsv_end(Conditions, Condition);
  Conditions[InstrZ].Name = NewName;
  Conditions[InstrZ++].Code = NewCode;
}

static void InitFields(void)
{
  InstTable = CreateInstTable(203);

  AddInstTable(InstTable, "LD" ,    G_LD,               DecodeLD);
  AddInstTable(InstTable, "LD.B",   G_LD_B,             DecodeLD);
  AddInstTable(InstTable, "LD.W",   G_LD_W,             DecodeLD);
  AddInstTable(InstTable, "LD.SB",  G_LD_SB,            DecodeLD);
  AddInstTable(InstTable, "LD.SW",  G_LD_SW,            DecodeLD);
  AddInstTable(InstTable, "PUSH",   G_PUSH,             DecodePUSH_POP);
  AddInstTable(InstTable, "POP" ,   G_POP,              DecodePUSH_POP);
  AddInstTable(InstTable, "IN"  ,   bus_cmd_read,       DecodeIN_OUT);
  AddInstTable(InstTable, "IN.B",   bus_cmd_read_b,     DecodeIN_OUT);
  AddInstTable(InstTable, "IN.W",   bus_cmd_read_w,     DecodeIN_OUT);
  AddInstTable(InstTable, "OUT" ,   bus_cmd_write,      DecodeIN_OUT);
  AddInstTable(InstTable, "OUT.B",  bus_cmd_write_b,    DecodeIN_OUT);
  AddInstTable(InstTable, "OUT.W",  bus_cmd_write_w,    DecodeIN_OUT);
  AddInstTable(InstTable, "RET" ,   G_RET,              DecodeRET);
  AddInstTable(InstTable, "JP" ,    G_JP,               DecodeJump);
  AddInstTable(InstTable, "CALL",   G_CALL,             DecodeJump);
  AddInstTable(InstTable, "JR" ,    G_JR,               DecodeJump);
  AddInstTable(InstTable, "DJNZ",   G_DJNZ,             DecodeJump);
  AddInstTable(InstTable, "RST",    G_RST,              DecodeRST);

  AddInstTable(InstTable, "INVF",   G_INVF,             DecodeImm1);
  AddInstTable(InstTable, "SETF",   G_SETF,             DecodeImm1);
  AddInstTable(InstTable, "CLRF",   G_CLRF,             DecodeImm1);
  AddInstTable(InstTable, "TSTF",   G_TSTF,             DecodeImm1);

  AddInstTable(InstTable, "EXTN.W", G_EXTN_W,           DecodeReg1);
  AddInstTable(InstTable, "EXTN.B", G_EXTN_B,           DecodeReg1);
  AddInstTable(InstTable, "CPL",    G_CPL,              DecodeReg1);
  AddInstTable(InstTable, "NEG",    G_NEG,              DecodeReg1);

  AddInstTable(InstTable, "EX",     G_EX,               DecodeReg2);

  AddInstTable(InstTable, "ADD",    G_ADD,              DecodeALU);
  AddInstTable(InstTable, "SUB",    G_SUB,              DecodeALU);
  AddInstTable(InstTable, "MUL",    G_MUL,              DecodeALU);
  AddInstTable(InstTable, "DIV",    G_DIV,              DecodeALU);
  AddInstTable(InstTable, "AND",    G_AND,              DecodeALU);
  AddInstTable(InstTable, "OR",     G_OR,               DecodeALU);
  AddInstTable(InstTable, "XOR",    G_XOR,              DecodeALU);
  AddInstTable(InstTable, "CP",     G_CP,               DecodeALU);

  AddInstTable(InstTable, "SRA",    G_SRA,              DecodeShift);
  AddInstTable(InstTable, "SRL",    G_SRL,              DecodeShift);
  AddInstTable(InstTable, "SL",     G_SL,               DecodeShift);
  AddInstTable(InstTable, "RLC",    G_RLC,              DecodeShift);

  AddInstTable(InstTable, "REG",    0,                  CodeREG);
  AddInstTable(InstTable, "DEFM",   0,                  ModIntel);
  AddInstTable(InstTable, "DEFB",   0,                  ModIntel);
  AddInstTable(InstTable, "DEFW",   0,                  ModIntel);

  InstrZ = 0;
  AddCondition("NZ", reg_flag_zero      | reg_flag_not);
  AddCondition("Z" , reg_flag_zero);
  AddCondition("NC", reg_flag_carry     | reg_flag_not);
  AddCondition("C" , reg_flag_carry);
  AddCondition("PO", reg_flag_parity    | reg_flag_not);
  AddCondition("NV", reg_flag_overflow  | reg_flag_not);
  AddCondition("PE", reg_flag_parity);
  AddCondition("V" , reg_flag_overflow);
  AddCondition("P" , reg_flag_sign      | reg_flag_not);
  AddCondition("NS", reg_flag_sign      | reg_flag_not);
  AddCondition("M" , reg_flag_sign);
  AddCondition("S" , reg_flag_sign);
  AddCondition(NULL, 0);

  InstrZ = 0;
  AddFixed("NOP"   , e_core_mask_all    , I_NOP());
  AddFixed("HALT"  , e_core_mask_all    , I_HALT());
  AddFixed("INVL"  , e_core_mask_all    , I_INV());

}

static void DeinitFields(void)
{
  order_array_free(Conditions);
  order_array_free(FixedOrders);

  DestroyInstTable(InstTable);
}

/*=========================================================================*/

static void MakeCode_Z80(void)
{
  CodeLen = 0;
  DontPrint = False;
  OpSize = eSymbolSizeUnknown;

  /* To Be Ignored: */

  if (Memo("")) return;

  /* evtl. Datenablage */

  if (DecodeIntelPseudo(False)) return;

  switch (ArgCnt) {
  case 0:
    dprint("%s\n", OpPart.str.p_str);
    break;
  case 1:
    dprint("%s %s\n", OpPart.str.p_str, ArgStr[1].str.p_str);
    break;
  case 2:
    dprint("%s %s %s\n", OpPart.str.p_str, ArgStr[1].str.p_str, ArgStr[2].str.p_str);
    break;
  case 3:
    dprint("%s %s %s %s\n", OpPart.str.p_str, ArgStr[1].str.p_str, ArgStr[2].str.p_str,
           ArgStr[3].str.p_str);
    break;
  default:
    dprint("%s %s %s %s ...\n", OpPart.str.p_str, ArgStr[1].str.p_str, ArgStr[2].str.p_str,
           ArgStr[3].str.p_str);
    break;
  }
  if (!LookupInstTable(InstTable, OpPart.str.p_str))
    WrStrErrorPos(ErrNum_UnknownInstruction, &OpPart);
  switch (CodeLen) {
  case 0:
    dprint("%06x  *** empty ***\n", (unsigned)EProgCounter());
    break;
  case 1:
  case 2:
    dprint("%06x  %02x %02x\n", (unsigned)EProgCounter(),
           BAsmCode[0], BAsmCode[1]);
    break;
  case 3:
  case 4:
    dprint("%06x  %02x %02x %02x %02x\n", (unsigned)EProgCounter(),
           BAsmCode[0], BAsmCode[1], BAsmCode[2], BAsmCode[3]);
    break;
  case 5:
  case 6:
    dprint("%06x  %02x %02x %02x %02x %02x %02x\n", (unsigned)EProgCounter(),
           BAsmCode[0], BAsmCode[1], BAsmCode[2], BAsmCode[3],
           BAsmCode[4], BAsmCode[5]);
    break;
  case 7:
  case 8:
    dprint("%06x  %02x %02x %02x %02x %02x %02x %02x %02x\n", (unsigned)EProgCounter(),
           BAsmCode[0], BAsmCode[1], BAsmCode[2], BAsmCode[3],
           BAsmCode[4], BAsmCode[5], BAsmCode[6], BAsmCode[7]);
    break;
  default:
    dprint("%06x  %02x %02x %02x %02x %02x %02x %02x %02x ...\n", (unsigned)EProgCounter(),
           BAsmCode[0], BAsmCode[1], BAsmCode[2], BAsmCode[3],
           BAsmCode[4], BAsmCode[5], BAsmCode[6], BAsmCode[7]);
    break;
  }
}

static void InitCode_Z80(void)
{
}

static Boolean IsDef_Z80(void)
{
  return Memo("PORT") || Memo("REG");
}

/* Treat special case of AF' which is no quoting: */

static Boolean QualifyQuote_Z80(const char *pStart, const char *pQuotePos)
{
  if ((*pQuotePos == '\'')
   && (pQuotePos >= pStart + 2)
   && (as_toupper(*(pQuotePos - 2)) == 'A')
   && (as_toupper(*(pQuotePos - 1)) == 'F'))
    return False;
  return True;
}

/*!------------------------------------------------------------------------
 * \fn     DissectReg_H80(char *p_dest, size_t dest_size, tRegInt value, tSymbolSize inp_size)
 * \brief  dissect register symbols - Z80 variant
 * \param  p_dest destination buffer
 * \param  dest_size destination buffer size
 * \param  value numeric register value
 * \param  inp_size register size
 * ------------------------------------------------------------------------ */

static void DissectReg_H80(char *p_dest, size_t dest_size, tRegInt value, tSymbolSize inp_size)
{
  as_snprintf(p_dest, dest_size, "r%d", value);
}

/*!------------------------------------------------------------------------
 * \fn     InternSymbol_Z80(char *p_arg, TempResult *p_result)
 * \brief  handle built-in (register) symbols for Z80
 * \param  p_arg source argument
 * \param  p_result result buffer
 * ------------------------------------------------------------------------ */

static void InternSymbol_H80(char *p_arg, TempResult *p_result)
{
  name_table_t *p = RegNames;
  while (p->name != NULL && as_strcasecmp(p->name, p_arg) != 0) {
    p++;
  }
  if (p->name != NULL) {
    p_result->Typ = TempReg;
    p_result->DataSize = eSymbolSize8Bit;
    p_result->Contents.RegDescr.Reg = p->number;
    p_result->Contents.RegDescr.Dissect = DissectReg_H80;
    p_result->Contents.RegDescr.compare = NULL;
  }
}

static Boolean ChkMoreOneArg(void)
{
  return (ArgCnt > 1);
}

/*!------------------------------------------------------------------------
 * \fn     SwitchTo_Z80(void *p_user)
 * \brief  switch to Z80 target
 * \param  p_user properties of CPU
 * ------------------------------------------------------------------------ */

static void SwitchTo_Z80(void *p_user)
{
  p_curr_cpu_props = (const cpu_props_t*)p_user;

  TurnWords = False;
  SetIntConstMode(eIntConstModeIntel);
  SetIsOccupiedFnc = ChkMoreOneArg;

  PCSymbol = "$"; HeaderID = 0x51; NOPCode = 0x00;
  DivideChars = ","; HasAttrs = False;

  ValidSegs = 1 << SegCode;
  Grans[SegCode] = 1; ListGrans[SegCode] = 1; SegInits[SegCode] = 0;

  SegLimits[SegCode] = 0xffffffu;

  /* Gameboy Z80 does not have I/O space, and no IX/IY, do not test for them and allow as normal symbols: */

  ValidSegs |= 1 << SegIO;
  Grans[SegIO  ] = 1; ListGrans[SegIO  ] = 1; SegInits[SegIO  ] = 0;
  SegLimits[SegIO  ] = 0xffffu;

  MakeCode = MakeCode_Z80;
  IsDef = IsDef_Z80;
  QualifyQuote = QualifyQuote_Z80;
  InternSymbol = InternSymbol_H80;
  SwitchFrom = DeinitFields; InitFields();
  DissectReg = DissectReg_H80;

  asmerr_warn_relative_add();
}

static const cpu_props_t cpu_props[] =
{
  { "H80"       , e_core_h80  , e_core_flag_none }
};

void codeh80_init(void)
{
  const cpu_props_t *p_props;

  for (p_props = cpu_props; p_props < cpu_props + as_array_size(cpu_props); p_props++)
    (void)AddCPUUser(p_props->p_name, SwitchTo_Z80, (void*)p_props, NULL);

  AddInitPassProc(InitCode_Z80);
}
