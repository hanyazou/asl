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

/*-------------------------------------------------------------------------*/
/* Praefixtyp */

typedef enum
{
  Pref_IN_N, Pref_IN_W, Pref_IB_W, Pref_IW_W, Pref_IB_N,
  Pref_IN_LW, Pref_IB_LW, Pref_IW_LW, Pref_IW_N
} PrefType;

typedef enum
{
  ePrefixNone,
  ePrefixW,   /* word processing */
  ePrefixLW,  /* long word processing */
  ePrefixIB,  /* one byte more in argument */
  ePrefixIW   /* one word more in argument */
} tOpPrefix;

typedef enum
{
  e_core_h80,
} cpu_core_t;

typedef enum
{
  e_core_mask_h80 = 1 << e_core_h80,
  e_core_mask_all = e_core_mask_h80,
} cpu_core_mask_t;

typedef enum
{
  e_core_flag_none = 0,
  e_core_flag_i_8bit = 1 << 0,
  e_core_flag_no_xio = 1 << 1
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

#define LWordFlagName  "INLWORDMODE"

#define ModNone (-1)
#define ModReg 1
#define ModIndReg 3
#define ModImm 4

#define ModAbs 5
#define ModSPRel 8
#define ModIndReg8 9
#define ModSPAdd 10
#define ModHLInc 11
#define ModHLDec 12
#define ModIOAbs 13
#define ModImmIsAbs 14
#define ModMB 15

#define MModReg (1 << ModReg)
#define MModIndReg (1 << ModIndReg)
#define MModImm (1 << ModImm)

#define MModAbs (1 << ModAbs)
#define MModSPRel (1 << ModSPRel)
#define MModIndReg8 (1 << ModIndReg8)
#define MModSPAdd (1 << ModSPAdd)
#define MModHLInc (1 << ModHLInc)
#define MModHLDec (1 << ModHLDec)
#define MModIOAbs (1 << ModIOAbs)
#define MModImmIsAbs (1 << ModImmIsAbs)
#define MModMB (1 << ModMB)

/* These masks deliberately omit the (special)
   Sharp/Gameboy addressing modes: */

#define MModNoImm (MModReg | MModIndReg)
#define MModAll (MModReg | MModIndReg | MModImm)

#define IXPrefix 0xdd
#define IYPrefix 0xfd

#define AccReg 7
#define MReg 6
#define HReg 4
#define LReg 5

#define DEReg 1
#define HLReg 2
#define SPReg 3

/*-------------------------------------------------------------------------*/
/* Instruktionsgruppendefinitionen */

typedef struct
{
  cpu_core_mask_t core_mask;
  Word Code;
} BaseOrder;

typedef struct
{
  const char *Name;
  Byte Code;
} Condition;

/*-------------------------------------------------------------------------*/

static Byte PrefixCnt;
static Byte AdrPart;
static tSymbolSize OpSize;
static Byte AdrVals[4];
static ShortInt AdrMode;

static BaseOrder *FixedOrders;
static BaseOrder *AccOrders;
static BaseOrder *HLOrders;
static Condition *Conditions;

static const cpu_props_t *p_curr_cpu_props;

static Boolean MayLW,             /* Instruktion erlaubt 32 Bit */
               ExtFlag;           /* Prozessor im 4GByte-Modus ? */

static PrefType LastPrefix;       /* von der letzten Anweisung generierter Praefix */

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

    { "fl", 16 },
    { "pc", 17 },
    { "sp", 18 },
    { "bp", 19 },
    { NULL },
};
static const char Reg8Names[] = "BCDEHL*A";
static int Reg16Cnt;
static const char Reg16Names[][3] = { "BC", "DE", "HL", "SP", "IX", "IY" };

static Boolean ExtendPrefix(PrefType *Dest, tOpPrefix AddPrefix)
{
  Byte SPart,IPart;

  switch (*Dest)
  {
    case Pref_IB_N:
    case Pref_IB_W:
    case Pref_IB_LW:
      IPart = 1;
      break;
    case Pref_IW_N:
    case Pref_IW_W:
    case Pref_IW_LW:
      IPart = 2;
      break;
    default:
      IPart = 0;
  }

  switch (*Dest)
  {
    case Pref_IN_W:
    case Pref_IB_W:
    case Pref_IW_W:
      SPart = 1;
      break;
    case Pref_IN_LW:
    case Pref_IB_LW:
    case Pref_IW_LW:
      SPart = 2;
      break;
    default:
      SPart = 0;
  }

  switch (AddPrefix)
  {
    case ePrefixW:
      SPart = 1; break;
    case ePrefixLW:
      SPart = 2; break;
    case ePrefixIB:
      IPart = 1; break;
    case ePrefixIW:
      IPart = 2; break;
    default:
      return False;
  }

  switch ((IPart << 4) | SPart)
  {
    case 0x00:
      *Dest = Pref_IN_N;
      break;
    case 0x01:
      *Dest = Pref_IN_W;
      break;
    case 0x02:
      *Dest = Pref_IN_LW;
      break;
    case 0x10:
      *Dest = Pref_IB_N;
      break;
    case 0x11:
      *Dest = Pref_IB_W;
      break;
    case 0x12:
      *Dest = Pref_IB_LW;
      break;
    case 0x20:
      *Dest = Pref_IW_N;
      break;
    case 0x21:
      *Dest = Pref_IW_W;
      break;
    case 0x22:
      *Dest = Pref_IW_LW;
      break;
  }

  return True;
}

/*--------------------------------------------------------------------------*/
/* Code fuer Praefix bilden */

static void GetPrefixCode(PrefType inp, Byte *b1 ,Byte *b2)
{
  int z;

  z = ((int)inp) - 1;
  *b1 = 0xdd + ((z & 4) << 3);
  *b2 = 0xc0 + (z & 3);
}

/*--------------------------------------------------------------------------*/
/* DD-Praefix addieren, nur EINMAL pro Instruktion benutzen! */

static void ChangeDDPrefix(tOpPrefix Prefix)
{
  PrefType ActPrefix;
  int z;

  ActPrefix = LastPrefix;
  if (ExtendPrefix(&ActPrefix, Prefix))
    if (LastPrefix != ActPrefix)
    {
      if (LastPrefix != Pref_IN_N) RetractWords(2);
      for (z = PrefixCnt - 1; z >= 0; z--) BAsmCode[2 + z] = BAsmCode[z];
      PrefixCnt += 2;
      GetPrefixCode(ActPrefix, BAsmCode + 0, BAsmCode + 1);
    }
}

/*!------------------------------------------------------------------------
 * \fn     EvalAbsAdrExpression(const tStrComp *pArg, tEvalResult *pEvalResult)
 * \brief  evaluate absolute address, range is targent-dependant
 * \param  pArg source argument
 * \param  pEvalResult sideband params
 * \return address value
 * ------------------------------------------------------------------------ */

static LongWord EvalAbsAdrExpression(const tStrComp *pArg, tEvalResult *pEvalResult)
{
  if (ExtFlag)
    return EvalStrIntExpressionWithResult(pArg, Int32, pEvalResult);
  else
    return EvalStrIntExpressionWithResult(pArg, UInt16, pEvalResult);
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

  /* special case for GameBoy/Sharp: FF00 always allowed, independent of radix: */

  if (!as_strcasecmp(p_arg->str.p_str, "FF00"))
  {
    as_tempres_set_int(p_res, 0xff00);
    return e_eval_ok;
  }

  switch (DecodeReg(p_arg, &this_reg, &this_reg_size, eSymbolSizeUnknown, False))
  {
    case eIsReg:
      if ((p_z80_eval_cb_data->addr_reg != 0xff)
       || !as_eval_cb_data_stack_plain_add(p_data->p_stack))
      {
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
  Integer AdrInt;
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

    if (z80_eval_cb_data.addr_reg == 0xff) {
      /* no register: absolute */
      // TODO
      goto found;
    } else {
      AdrMode = ModIndReg;
      AdrPart = z80_eval_cb_data.addr_reg;
      goto found;
    }
  }

  /* ...immediate */

  if (!(ModeMask & MModImm))
    goto inv_mode;

  switch (OpSize) {
    case eSymbolSize8Bit:
      AdrVals[0] = EvalStrIntExpression(pArg, Int8, &OK);
      if (OK) {
        AdrMode = ModImm;
        AdrCnt = 1;
      }
      break;
    case eSymbolSize16Bit:
      AdrInt = EvalStrIntExpression(pArg, Int16, &OK);
      if (OK) {
        AdrVals[0] = Lo(AdrInt);
        AdrVals[1] = Hi(AdrInt);
        AdrMode = ModImm;
        AdrCnt = 2;
      }
      break;
    case eSymbolSizeUnknown:
      {
        LongWord ImmVal = EvalStrIntExpression(pArg, Int32, &OK);
        if (OK)
        {
          AdrVals[0] = Lo(ImmVal);
          AdrVals[1] = Hi(ImmVal);
          AdrMode = ModImm;
          AdrCnt = 2;
          if (ImmVal <= 0xfffful);
          else
          {
            AdrVals[AdrCnt++] = (ImmVal >> 16) & 0xff;
            if (ImmVal <= 0xfffffful)
              ChangeDDPrefix(ePrefixIB);
            else
            {
              AdrVals[AdrCnt++] = (ImmVal >> 24) & 0xff;
              ChangeDDPrefix(ePrefixIW);
            }
          }
        }
      }
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

/*!------------------------------------------------------------------------
 * \fn     store_prefix(prefix_store_t *p_store, Byte old_prefix_cnt)
 * \brief  check whether another (index) prefix was added, and store it
 * \param  p_store place to store
 * \param  old_prefix_cnt prefix cound before possible addition
 * ------------------------------------------------------------------------ */

typedef struct
{
  Byte cnt, value;
  Boolean present;
} prefix_store_t;

/*-------------------------------------------------------------------------*/
/* Bedingung entschluesseln */

static Boolean DecodeCondition(const char *Name, int *Erg)
{
  int z;

  for (z = 0; Conditions[z].Name; z++)
    if (!as_strcasecmp(Conditions[z].Name, Name))
    {
      *Erg = Conditions[z].Code;
      return True;
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
  if (!((core_mask >> p_curr_cpu_props->core) & 1))
  {
    WrStrErrorPos(ErrNum_InstructionNotSupported, &OpPart);
    return False;
  }
  return True;
}

/*!------------------------------------------------------------------------
 * \fn     append_to_prefixes(Word code)
 * \brief  append oe or two byte opcode to existing prefixes
 * \param  code code to append (2 byte if MSB != 0)
 * ------------------------------------------------------------------------ */

static void append_to_prefixes(Word code)
{
  if (Hi(code))
    BAsmCode[PrefixCnt++] = Hi(code);
  BAsmCode[PrefixCnt++] = Lo(code);
}

/*!------------------------------------------------------------------------
 * \fn     DecodeFixed(Word Index)
 * \brief  handle instructions without arguments
 * \param  Index * to instruction description
 * ------------------------------------------------------------------------ */

static void DecodeFixed(Word Index)
{
  BaseOrder *POrder = FixedOrders + Index;

  if (ChkArgCnt(0, 0)
   && chk_core_mask(POrder->core_mask))
  {
    append_to_prefixes(POrder->Code);
    CodeLen = PrefixCnt;
  }
}

/*!------------------------------------------------------------------------
 * \fn     DecodeLD(size)
 * \brief  handle LD instruction
 * \param  LD, LD.B or LD.W
 * ------------------------------------------------------------------------ */

static void DecodeLD(Word size)
{
  if (!ChkArgCnt(2, 2))
      return;

  switch (size) {
  case 0:
    OpSize = eSymbolSizeUnknown;
    break;
  case 1:
    OpSize = eSymbolSize8Bit;
    break;
  case 2:
    OpSize = eSymbolSize16Bit;
    break;
  }

  DecodeAdr(&ArgStr[1], MModReg | MModIndReg);
  switch (AdrMode) {
  case ModReg:
    DecodeAdr(&ArgStr[2], MModReg | MModIndReg | MModImm);
    switch (AdrMode) {
    case ModReg:    /* LD R, R */
      break;
    case ModIndReg: /* LD R, (R) */
      break;
    case ModImm:    /* LD R, imm */
      break;
    default:
      WrError(ErrNum_InvAddrMode);
    }
    break;
  case ModIndReg:
    DecodeAdr(&ArgStr[2], MModReg);
    switch (AdrMode) {
    case ModReg:    /* LD (R), R */
      break;
    default:
      WrError(ErrNum_InvAddrMode);
    }
    break;
  default:
    WrError(ErrNum_InvAddrMode);
    break;
  }
}

static void DecodeALU(Word Code)
{
  if (!ChkArgCnt(3, 3))
    return;

  DecodeAdr(&ArgStr[1], MModReg);
  if (AdrMode != ModReg) {
    return;
  }
  DecodeAdr(&ArgStr[2], MModReg);
  if (AdrMode != ModReg) {
    return;
  }
  DecodeAdr(&ArgStr[3], MModReg);
  if (AdrMode != ModReg) {
    return;
  }

  CodeLen = PrefixCnt;
  BAsmCode[CodeLen++] = 0xff;  // TODO
  BAsmCode[CodeLen++] = 0xff;
}

static void DecodeADD(Word Index)
{
  UNUSED(Index);

  if (ArgCnt == 2) {
    DecodeAdr(&ArgStr[1], MModReg);
    if (AdrMode != ModReg) {
      WrError(ErrNum_InvAddrMode);
      return;
    }
    OpSize = eSymbolSize8Bit;
    DecodeAdr(&ArgStr[2], MModImm);
    if (AdrMode != ModImm) {
      if (AdrMode != ModNone) {
        WrError(ErrNum_InvAddrMode);
      }
      return;
    }

    // TODO
    BAsmCode[CodeLen++] = 0xff;

  } else
  if (ArgCnt == 3) {
    DecodeALU(Index);
  } else {
    ChkArgCnt(2, 2);
  }
}

static void DecodeShift(Word Code)
{
  Byte reg_num = 0;
  int mem_arg_index;

  if (!ChkArgCnt(1, 1))
    return;

  mem_arg_index = 1;

  /* now decode the 'official argument': */

  OpSize = 0;
  DecodeAdr(&ArgStr[mem_arg_index], MModReg);
  if (AdrMode != ModReg)
    return;

  /* replace AdrPart for undocumented version.  Addressing mode must be IXd/IYd: */

  if (ArgCnt >= 2)
  {
    if ((AdrPart != 6) || (PrefixCnt != 1))
    {
      WrStrErrorPos(ErrNum_InvAddrMode, &ArgStr[mem_arg_index]);
      return;
    }
    AdrPart = reg_num;
  }

  /* assemble instruction: */

  CodeLen = PrefixCnt;
  BAsmCode[CodeLen++] = 0xcb;
  AppendAdrVals();
  BAsmCode[CodeLen++] = (Code << 3) | AdrPart;
}

static void DecodeBit(Word Code)
{
  Byte reg_num = 0;
  int mem_arg_index, bit_arg_index;
  Boolean ok;

  /* extra undocumented dest register is not allowed for BIT */

  if (!ChkArgCnt(1, (Code != 0) ? 3 : 2))
    return;

  mem_arg_index = 2;
  bit_arg_index = 1;

  /* now decode the 'official arguments': */

  OpSize = 0;
  DecodeAdr(&ArgStr[mem_arg_index], MModReg);
  if (AdrMode != ModReg)
    return;

  /* forbid IXL..IYU: */

  if ((PrefixCnt > 0) && (AdrPart != 6))
  {
    WrStrErrorPos(ErrNum_InvAddrMode, &ArgStr[mem_arg_index]);
    return;
  }

  /* parse bit # and form machine code: */

  Code = ((Code + 1) << 6) | (EvalStrIntExpression(&ArgStr[bit_arg_index], UInt3, &ok) << 3);
  if (!ok)
    return;

  /* replace AdrPart for undocumented version.  Addressing mode must be IXd/IYd: */

  if (ArgCnt >= 3)
  {
    if ((AdrPart != 6) || (PrefixCnt != 1))
    {
      WrStrErrorPos(ErrNum_InvAddrMode, &ArgStr[mem_arg_index]);
      return;
    }
    AdrPart = reg_num;
  }

  /* assemble instruction: */

  CodeLen = PrefixCnt;
  BAsmCode[CodeLen++] = 0xcb;
  AppendAdrVals();
  BAsmCode[CodeLen++] = Code | AdrPart;
}

/*!------------------------------------------------------------------------
 * \fn     DecodePUSH_POP(Word Code)
 * \brief  handle PUSH/POP instructions
 * \param  Code machine code (4 = PUSH family, 0 = POP family)
 * ------------------------------------------------------------------------ */

static void DecodePUSH_POP(Word Code)
{
  if (!ChkArgCnt(1, 1))
    return;
  DecodeAdr(&ArgStr[1], MModReg);
  if (AdrMode != ModReg) {
    if (AdrMode != ModNone) {
      WrError(ErrNum_InvAddrMode);
    }
    return;
  }

  // TODO
  CodeLen = PrefixCnt;
  BAsmCode[CodeLen++] = 0xfd;
  BAsmCode[CodeLen++] = 0xf5;
}

static void DecodeIN_OUT(Word IsOUT)
{
  if (!ChkArgCnt(2, 2))
    return;

  const tStrComp *pPortArg = IsOUT ? &ArgStr[1] : &ArgStr[2];
  const tStrComp *pRegArg = IsOUT ? &ArgStr[2] : &ArgStr[1];

  OpSize = 0;
  DecodeAdr(pPortArg, MModIndReg);
  if (AdrMode != ModIndReg) {
    return;
  }
  DecodeAdr(pRegArg, MModReg);
  if (AdrMode != ModReg) {
    return;
  }

  // TODO
  if (IsOUT) {
      CodeLen = 2;
      BAsmCode[0] = 0xed;
      BAsmCode[1] = 0x40 + (AdrPart << 3);
  } else {
      CodeLen = 2;
      BAsmCode[0] = 0xed;
      BAsmCode[1] = 0x40 + (AdrPart << 3);
  }
}

static void DecodeRET(Word Code)
{
  int Cond;

  UNUSED(Code);

  if (ArgCnt == 0)
  {
    CodeLen = PrefixCnt + 1;
    BAsmCode[PrefixCnt] = 0xc9;
  }
  else if (!ChkArgCnt(0, 1));
  else if (!DecodeCondition(ArgStr[1].str.p_str, &Cond)) WrStrErrorPos(ErrNum_UndefCond, &ArgStr[1]);
  else
  {
    CodeLen = PrefixCnt + 1;
    BAsmCode[PrefixCnt] = 0xc0 + (Cond << 3);
  }
}

static IntType get_jr_dist(LongWord dest, LongInt *p_dist)
{
  *p_dist = dest - (EProgCounter() + 2);
  if (RangeCheck(*p_dist, SInt8))
    return SInt8;
  *p_dist -= 2;
  if (RangeCheck(*p_dist, SInt16))
    return SInt16;
  (*p_dist)--;
  if (RangeCheck(*p_dist, SInt24))
    return SInt24;
  return UInt0;
}

static void DecodeJP(Word Code)
{
  int Cond;

  UNUSED(Code);

  switch (ArgCnt)
  {
    case 1:
      Cond = 1;
      break;
    case 2:
      if (!DecodeCondition(ArgStr[1].str.p_str, &Cond))
      {
        WrStrErrorPos(ErrNum_UndefCond, &ArgStr[1]);
        return;
      }
      Cond <<= 3;
      break;
    default:
      (void)ChkArgCnt(1, 2);
      return;
  }

  DecodeAdr(&ArgStr[ArgCnt], MModIndReg);
  if (AdrMode != ModIndReg) {
    return;
  }

  // TODO
}

static void DecodeCALL(Word Code)
{
  Boolean OK;
  int Condition;

  UNUSED(Code);

  switch (ArgCnt)
  {
    case 1:
      Condition = 9;
      OK = True;
      break;
    case 2:
      OK = DecodeCondition(ArgStr[1].str.p_str, &Condition);
      if (OK)
        Condition <<= 3;
      else
        WrStrErrorPos(ErrNum_UndefCond, &ArgStr[1]);
      break;
    default:
      (void)ChkArgCnt(1, 2);
      OK = False;
  }

  if (OK)
  {
    LongWord AdrLong;
    tEvalResult EvalResult;

    AdrLong = EvalAbsAdrExpression(&ArgStr[ArgCnt], &EvalResult);
    if (EvalResult.OK)
    {
      if (AdrLong > 0xfffffful)
      {
        ChangeDDPrefix(ePrefixIW);
        CodeLen = PrefixCnt;
        BAsmCode[CodeLen++] = 0xc4 + Condition;
        BAsmCode[CodeLen++] = Lo(AdrLong);
        BAsmCode[CodeLen++] = Hi(AdrLong);
        BAsmCode[CodeLen++] = Hi(AdrLong >> 8);
        BAsmCode[CodeLen++] = Hi(AdrLong >> 16);
      }
      else if (AdrLong > 0xfffful)
      {
        ChangeDDPrefix(ePrefixIB);
        CodeLen = PrefixCnt;
        BAsmCode[CodeLen++] = 0xc4 + Condition;
        BAsmCode[CodeLen++] = Lo(AdrLong);
        BAsmCode[CodeLen++] = Hi(AdrLong);
        BAsmCode[CodeLen++] = Hi(AdrLong >> 8);
      }
      else
      {
        CodeLen = PrefixCnt;
        BAsmCode[CodeLen++] = 0xc4 + Condition;
        BAsmCode[CodeLen++] = Lo(AdrLong);
        BAsmCode[CodeLen++] = Hi(AdrLong);
      }
    }
  }
}

static void encode_jr_core(IntType dist_size, Byte condition, LongInt dist)
{
  switch (dist_size)
  {
    case SInt8:
      CodeLen = 2;
      BAsmCode[0] = condition << 3;
      BAsmCode[1] = dist & 0xff;
      break;
    case SInt16:
      CodeLen = 4;
      BAsmCode[0] = 0xdd;
      BAsmCode[1] = condition << 3;
      BAsmCode[2] = dist & 0xff;
      BAsmCode[3] = (dist >> 8) & 0xff;
      break;
    case SInt24:
      CodeLen = 5;
      BAsmCode[0] = 0xfd;
      BAsmCode[1] = condition << 3;
      BAsmCode[2] = dist & 0xff;
      BAsmCode[3] = (dist >> 8) & 0xff;
      BAsmCode[4] = (dist >> 16) & 0xff;
      break;
    default:
      break;
  }
}

static void DecodeJR(Word Code)
{
  Boolean OK;
  int Condition;
  LongWord dest;
  tEvalResult EvalResult;
  LongInt dist;
  IntType dist_type;

  UNUSED(Code);

  switch (ArgCnt)
  {
    case 1:
      Condition = 3;
      OK = True;
      break;
    case 2:
      OK = DecodeCondition(ArgStr[1].str.p_str, &Condition);
      if (OK && (Condition > 3))
        OK = False;
      if (OK)
        Condition += 4;
      else
        WrStrErrorPos(ErrNum_UndefCond, &ArgStr[1]);
      break;
    default:
      (void)ChkArgCnt(1, 2);
      OK = False;
  }
  if (!OK)
    return;

  dest = EvalAbsAdrExpression(&ArgStr[ArgCnt], &EvalResult);
  if (!EvalResult.OK)
    return;

  dist_type = get_jr_dist(dest, &dist);
  if (dist_type == UInt0)
  {
    if (mFirstPassUnknownOrQuestionable(EvalResult.Flags))
      dist_type = SInt24;
    else
    {
      WrStrErrorPos(ErrNum_JmpDistTooBig, &ArgStr[ArgCnt]);
      return;
    }
  }

  encode_jr_core(dist_type, Condition, dist);
}

static void DecodeDJNZ(Word Code)
{
  UNUSED(Code);

  if (ChkArgCnt(1, 1))
  {
    tEvalResult EvalResult;
    LongInt AdrLInt;

    AdrLInt = EvalAbsAdrExpression(&ArgStr[1], &EvalResult);
    if (EvalResult.OK)
    {
      AdrLInt -= EProgCounter() + 2;
      if ((AdrLInt <= 0x7fl) & (AdrLInt >= -0x80l))
      {
        CodeLen = 2;
        BAsmCode[0] = 0x10;
        BAsmCode[1] = Lo(AdrLInt);
      }
      else
      {
        AdrLInt -= 2;
        if ((AdrLInt <= 0x7fffl) && (AdrLInt >= -0x8000l))
        {
          CodeLen = 4;
          BAsmCode[0] = 0xdd;
          BAsmCode[1] = 0x10;
          BAsmCode[2] = AdrLInt & 0xff;
          BAsmCode[3] = (AdrLInt >> 8) & 0xff;
        }
        else
        {
          AdrLInt--;
          if ((AdrLInt <= 0x7fffffl) && (AdrLInt >= -0x800000l))
          {
            CodeLen = 5;
            BAsmCode[0] = 0xfd;
            BAsmCode[1] = 0x10;
            BAsmCode[2] = AdrLInt & 0xff;
            BAsmCode[3] = (AdrLInt >> 8) & 0xff;
            BAsmCode[4] = (AdrLInt >> 16) & 0xff;
          }
          else WrError(ErrNum_JmpDistTooBig);
        }
      }
    }
  }
}

static void DecodeRST(Word Code)
{
  UNUSED(Code);

  if (ChkArgCnt(1, 1))
  {
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
    if (OK)
    {
      if ((AdrByte > 0x38) || (AdrByte & 7)) WrError(ErrNum_NotFromThisAddress);
      else
      {
        CodeLen = PrefixCnt + 1;
        BAsmCode[PrefixCnt] = 0xc7 + AdrByte;
      }
    }
  }
}

static void DecodeEI_DI(Word Code)
{
  if (ArgCnt == 0)
  {
    BAsmCode[0] = 0xf3 + Code;
    CodeLen = 1;
  }
  else if (ChkArgCnt(1, 1))
  {
    Boolean OK;

    BAsmCode[2] = EvalStrIntExpression(&ArgStr[1], UInt8, &OK);
    if (OK)
    {
      BAsmCode[0] = 0xdd;
      BAsmCode[1] = 0xf3 + Code;
      CodeLen = 3;
    }
  }
}

static void DecodeIM(Word Code)
{
  UNUSED(Code);

  if (ChkArgCnt(1, 1))
  {
    Byte AdrByte;
    Boolean OK;

    AdrByte = EvalStrIntExpression(&ArgStr[1], UInt2, &OK);
    if (OK)
    {
      if (AdrByte > 3) WrError(ErrNum_OverRange);
      else
      {
        if (AdrByte == 3)
          AdrByte = 1;
        else if (AdrByte >= 1)
          AdrByte++;
        CodeLen = 2;
        BAsmCode[0] = 0xed;
        BAsmCode[1] = 0x46 + (AdrByte << 3);
      }
    }
  }
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
  AddInstTable(InstTable, NewName, InstrZ++, DecodeFixed);
}

static void AddBit(const char *NName, Word Code)
{
  AddInstTable(InstTable, NName, Code, DecodeBit);
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

  AddInstTable(InstTable, "LD" ,  0, DecodeLD);
  AddInstTable(InstTable, "LD.B", 1, DecodeLD);
  AddInstTable(InstTable, "LD.W", 2, DecodeLD);
  AddInstTable(InstTable, "ADD", 0, DecodeADD);
  AddInstTable(InstTable, "PUSH", 4, DecodePUSH_POP);
  AddInstTable(InstTable, "POP" , 0, DecodePUSH_POP);
  AddInstTable(InstTable, "IN"  , 0, DecodeIN_OUT);
  AddInstTable(InstTable, "OUT" , 1, DecodeIN_OUT);
  AddInstTable(InstTable, "RET" , 0, DecodeRET);
  AddInstTable(InstTable, "JP" , 0, DecodeJP);
  AddInstTable(InstTable, "CALL", 0, DecodeCALL);
  AddInstTable(InstTable, "JR" , 0, DecodeJR);
  AddInstTable(InstTable, "DJNZ", 0, DecodeDJNZ);
  AddInstTable(InstTable, "RST", 0, DecodeRST);
  AddInstTable(InstTable, "DI", 0, DecodeEI_DI);
  AddInstTable(InstTable, "EI", 8, DecodeEI_DI);
  AddInstTable(InstTable, "IM", 0, DecodeIM);
  AddInstTable(InstTable, "DEFM", 0, ModIntel);
  AddInstTable(InstTable, "DEFB", 0, ModIntel);
  AddInstTable(InstTable, "DEFW", 0, ModIntel);

  InstrZ = 0;
  AddCondition("NZ", 0);
  AddCondition("Z" , 1);
  AddCondition("NC", 2);
  AddCondition("C" , 3);
  AddCondition("PO", 4);
  AddCondition("NV", 4);
  AddCondition("PE", 5);
  AddCondition("V" , 5);
  AddCondition("P" , 6);
  AddCondition("NS", 6);
  AddCondition("M" , 7);
  AddCondition("S" , 7);
  AddCondition(NULL, 0);

  InstrZ = 0;
  AddFixed("NOP"   , e_core_mask_all       , 0x0000);
  AddFixed("HALT"  , e_core_mask_all       , 0x0076);

  AddInstTable(InstTable, "SUB", 0, DecodeALU);
  AddInstTable(InstTable, "AND", 0, DecodeALU);
  AddInstTable(InstTable, "OR",  0, DecodeALU);
  AddInstTable(InstTable, "XOR", 0, DecodeALU);
  AddInstTable(InstTable, "CP",  0, DecodeALU);

  AddInstTable(InstTable, "RLC",  0, DecodeShift);
  AddInstTable(InstTable, "RRC",  1, DecodeShift);
  AddInstTable(InstTable, "RL",   2, DecodeShift);
  AddInstTable(InstTable, "RR",   3, DecodeShift);
  AddInstTable(InstTable, "SLA",  4, DecodeShift);
  AddInstTable(InstTable, "SRA",  5, DecodeShift);
  AddInstTable(InstTable, "SRL",  7, DecodeShift);

  AddBit("BIT", 0);
  AddBit("RES", 1);
  AddBit("SET", 2);

  AddInstTable(InstTable, "REG" , 0, CodeREG);
}

static void DeinitFields(void)
{
  order_array_free(Conditions);
  order_array_free(FixedOrders);
  order_array_free(AccOrders);
  order_array_free(HLOrders);

  DestroyInstTable(InstTable);
}

/*=========================================================================*/

static void MakeCode_Z80(void)
{
  CodeLen = 0;
  DontPrint = False;
  PrefixCnt = 0;
  OpSize = 0xff;
  MayLW = False;

  /* To Be Ignored: */

  if (Memo("")) return;

  /* evtl. Datenablage */

  if (DecodeIntelPseudo(False)) return;

  if (!LookupInstTable(InstTable, OpPart.str.p_str))
    WrStrErrorPos(ErrNum_UnknownInstruction, &OpPart);
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
  // TODO
  printf("#### %s(%d)\n", __func__, __LINE__);

  switch (inp_size)
  {
    case eSymbolSize8Bit:
      if ((value & 0xf0) == (IXPrefix & 0xf0))
        as_snprintf(p_dest, dest_size, "%s%c", Reg16Names[4], (value & 1) ? 'L' : 'U');
      else if ((value & 0xf0) == (IYPrefix & 0xf0))
        as_snprintf(p_dest, dest_size, "%s%c", Reg16Names[5], (value & 1) ? 'L' : 'U');
      else if ((value < 8) && (value != 6))
        as_snprintf(p_dest, dest_size, "%c", Reg8Names[value]);
      else
        goto none;
      break;
    case eSymbolSize16Bit:
      if ((value & 0xf0) == (IXPrefix & 0xf0))
        as_snprintf(p_dest, dest_size, Reg16Names[4]);
      else if ((value & 0xf0) == (IYPrefix & 0xf0))
        as_snprintf(p_dest, dest_size, Reg16Names[5]);
      else if (value < 4)
        as_snprintf(p_dest, dest_size, "%s", Reg16Names[value]);
      else
        goto none;
      break;
    none:
    default:
      as_snprintf(p_dest, dest_size, "%d-%u", (int)inp_size, (unsigned)value);
  }
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
  Reg16Cnt = 6;

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
