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
#define ModReg8 1
#define ModReg16 2
#define ModIndReg16 3
#define ModImm 4
#define ModAbs 5
#define ModRef 6
#define ModInt 7
#define ModSPRel 8
#define ModIndReg8 9
#define ModSPAdd 10
#define ModHLInc 11
#define ModHLDec 12
#define ModIOAbs 13
#define ModImmIsAbs 14
#define ModMB 15

#define MModReg8 (1 << ModReg8)
#define MModReg16 (1 << ModReg16)
#define MModIndReg16 (1 << ModIndReg16)
#define MModImm (1 << ModImm)
#define MModAbs (1 << ModAbs)
#define MModRef (1 << ModRef)
#define MModInt (1 << ModInt)
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

#define MModNoImm (MModReg8 | MModReg16 | MModIndReg16 | MModAbs | MModRef | MModInt | MModSPRel)
#define MModAll (MModReg8 | MModReg16 | MModIndReg16 | MModImm | MModAbs | MModRef | MModInt | MModSPRel)

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
static Byte AdrPart, OpSize;
static LongWord AdrVal;
static LongWord adr_val_flags;
static Byte AdrVals[4];
static ShortInt AdrMode;

static BaseOrder *FixedOrders;
static BaseOrder *AccOrders;
static BaseOrder *HLOrders;
static Condition *Conditions;

static const cpu_props_t *p_curr_cpu_props;

static Boolean MayLW,             /* Instruktion erlaubt 32 Bit */
               ExtFlag,           /* Prozessor im 4GByte-Modus ? */
               LWordFlag;         /* 32-Bit-Verarbeitung ? */

static PrefType LastPrefix;       /* von der letzten Anweisung generierter Praefix */

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

/*--------------------------------------------------------------------------*/
/* IX/IY used ? */

static Boolean IndexPrefix(void)
{
  return   ((PrefixCnt > 0)
         && ((BAsmCode[PrefixCnt - 1] == IXPrefix)
          || (BAsmCode[PrefixCnt - 1] == IYPrefix)));
}

/*--------------------------------------------------------------------------*/
/* Wortgroesse ? */

static Boolean InLongMode(void)
{
  switch (LastPrefix)
  {
    case Pref_IN_W:
    case Pref_IB_W:
    case Pref_IW_W:
      return False;
    case Pref_IN_LW:
    case Pref_IB_LW:
    case Pref_IW_LW:
      return MayLW;
    default:
      return LWordFlag && MayLW;
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
 * \fn     DecodeReg8Core(const char *p_asc, Byte *p_ret)
 * \brief  parse 8 bit register
 * \param  p_asc source argument
 * \param  p_ret return buffer
 * \return true if valid register name
 * ------------------------------------------------------------------------ */

static Boolean DecodeReg8Core(const char *p_asc, Byte *p_ret)
{
  const char *p_pos;

  switch (strlen(p_asc))
  {
    case 1:
      p_pos = strchr(Reg8Names, as_toupper(p_asc[0]));
      if (!p_pos)
        return False;
      *p_ret = p_pos - Reg8Names;
      return (*p_ret != 6);
    case 3:
    {
      char ix = toupper(p_asc[1]);

      if ((toupper(p_asc[0]) != 'I')
       || ((ix != 'X') && (ix != 'Y')))
        return False;
      switch (toupper(p_asc[2]))
      {
        case 'L':
          *p_ret = 5 | (((ix == 'X') ? IXPrefix : IYPrefix) & 0xf0);
          return True;
        case 'H':
          goto ir_high;
        case 'U':
          goto ir_high;
        ir_high:
          *p_ret = 4 | (((ix == 'X') ? IXPrefix : IYPrefix) & 0xf0);
          return True;
        default:
          return False;
      }
    }
    default:
      return False;
  }
}

/*!------------------------------------------------------------------------
 * \fn     DecodeReg16Core(const char *p_asc, Byte *p_ret)
 * \brief  parse 16 bit register
 * \param  p_asc source argument
 * \param  p_ret return buffer
 * \return true if valid register name
 * ------------------------------------------------------------------------ */

static Boolean DecodeReg16Core(const char *p_asc, Byte *p_ret)
{
  int z;

  for (z = 0; z < Reg16Cnt; z++)
    if (!as_strcasecmp(p_asc, Reg16Names[z]))
    {
      if (z <= 3)
        *p_ret = z;
      else
        *p_ret = 2 /* = HL */ | (((z == 4) ? IXPrefix : IYPrefix) & 0xf0);
      return True;
    }
  return False;
}

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

static tRegEvalResult DecodeReg(const tStrComp *p_arg, Byte *p_ret, tSymbolSize *p_size, tSymbolSize req_size, Boolean must_be_reg)
{
  tRegEvalResult reg_eval_result;
  tEvalResult eval_result;
  tRegDescr reg_descr;

  if (DecodeReg8Core(p_arg->str.p_str, p_ret))
  {
    eval_result.DataSize = eSymbolSize8Bit;
    reg_eval_result = eIsReg;
  }
  else if (DecodeReg16Core(p_arg->str.p_str, p_ret))
  {
    eval_result.DataSize = eSymbolSize16Bit;
    reg_eval_result = eIsReg;
  }
  else
  {
    reg_eval_result = EvalStrRegExpressionAsOperand(p_arg, &reg_descr, &eval_result, eSymbolSizeUnknown, must_be_reg);
    if (reg_eval_result == eIsReg)
      *p_ret = reg_descr.Reg;
  }

  if (reg_eval_result == eIsReg)
  {
    if (!chk_reg_size(req_size, eval_result.DataSize))
    {
      WrStrErrorPos(ErrNum_InvOpSize, p_arg);
      reg_eval_result = must_be_reg ? eIsNoReg : eRegAbort;
    }
  }

  if (p_size) *p_size = eval_result.DataSize;
  return reg_eval_result;
}

static Boolean IsSym(char ch)
{
  return ((ch == '_')
       || ((ch >= '0') && (ch <= '9'))
       || ((ch >= 'A') && (ch <= 'Z'))
       || ((ch >= 'a') && (ch <= 'z')));
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

static void abs_2_adrvals(LongWord address)
{
  AdrVal = address;
  AdrVals[0] = address & 0xff;
  AdrVals[1] = (address >> 8) & 0xff;
  AdrCnt = 2;

  if (ExtFlag && (address > 0xfffful))
  {
    AdrVals[AdrCnt++] = (address >> 16) & 0xff;
    if (address <= 0xfffffful)
      ChangeDDPrefix(ePrefixIB);
    else
    {
      AdrVals[AdrCnt++] = ((address >> 24) & 0xff);
      ChangeDDPrefix(ePrefixIW);
    }
  }
}

static ShortInt DecodeAdr(const tStrComp *pArg, unsigned ModeMask)
{
  Integer AdrInt;
#if 0
  int z, l;
  LongInt AdrLong;
#endif
  Boolean OK, is_indirect;
  tEvalResult EvalResult;

  AdrMode = ModNone;
  AdrCnt = 0;
  AdrPart = 0;

  /* 0. Sonderregister */

  if (!as_strcasecmp(pArg->str.p_str, "R"))
  {
    AdrMode = ModRef;
    goto found;
  }

  if (!as_strcasecmp(pArg->str.p_str, "I"))
  {
    AdrMode = ModInt;
    goto found;
  }

  /* 1. 8/16 bit registers ? */

  switch (DecodeReg(pArg, &AdrPart, &EvalResult.DataSize, eSymbolSizeUnknown, False))
  {
    case eRegAbort:
      goto found;
    case eIsReg:
      if (AdrPart & 0xf0)
        BAsmCode[PrefixCnt++] = (AdrPart & 0xf0) | 0x0d;
      AdrPart &= (EvalResult.DataSize == eSymbolSize8Bit) ? 7 : 3;
      AdrMode = (EvalResult.DataSize == eSymbolSize8Bit) ? ModReg8 : ModReg16;
      goto found;
    default:
      break;
  }

  /* 2. SP+d8 (Gameboy specific) */

  if ((ModeMask & MModSPAdd)
   && (strlen(pArg->str.p_str) >= 4)
   && !as_strncasecmp(pArg->str.p_str, "SP", 2)
   && !IsSym(pArg->str.p_str[2]))
  {
    AdrVals[0] = EvalStrIntExpressionOffs(pArg, 2, SInt8, &OK);
    if (OK)
    {
      AdrCnt = 1;
      AdrMode = ModSPAdd;
    }
    goto found;
  }

  /* all types of indirect expressions (...): */

  is_indirect = IsIndirect(pArg->str.p_str);
  if (is_indirect || (ModeMask & MModImmIsAbs))
  {
    tStrComp arg;
    tEvalResult disp_eval_result;
    LongInt disp_acc;
    z80_eval_cb_data_t z80_eval_cb_data;

    /* strip outer braces and spaces */

    StrCompRefRight(&arg, pArg, !!is_indirect);
    StrCompShorten(&arg, !!is_indirect);
    KillPrefBlanksStrCompRef(&arg);
    KillPostBlanksStrComp(&arg);

    /* special cases: */

    if ((ModeMask & MModHLInc) && (!as_strcasecmp(arg.str.p_str, "HL+") || !as_strcasecmp(arg.str.p_str, "HLI")))
    {
      AdrMode = ModHLInc;
      goto found;
    }
    if ((ModeMask & MModHLDec) && (!as_strcasecmp(arg.str.p_str, "HL-") || !as_strcasecmp(arg.str.p_str, "HLD")))
    {
      AdrMode = ModHLDec;
      goto found;
    }

    /* otherwise, walk through the components : */

    as_eval_cb_data_ini(&z80_eval_cb_data.cb_data, h80_eval_cb);
    z80_eval_cb_data.addr_reg = 0xff;
    z80_eval_cb_data.addr_reg_size = eSymbolSizeUnknown;
    disp_acc = EvalStrIntExprWithResultAndCallback(&arg, Int32, &disp_eval_result, &z80_eval_cb_data.cb_data);
    if (!disp_eval_result.OK)
      goto found;

    /* now we have parsed the expression, see what we can do with it: */

    switch (z80_eval_cb_data.addr_reg)
    {
      /* no register: absolute */
      case 0xff:
      {
        LongWord address = disp_acc;

        if (ModeMask & MModAbs)
        {

          if (ExtFlag);
          else
          {
            if (!mFirstPassUnknownOrQuestionable(disp_eval_result.Flags)
             && !ChkRangeByType(disp_acc, UInt16, pArg))
              return AdrMode;
          }
          ChkSpace(SegCode, disp_eval_result.AddrSpaceMask);
          abs_2_adrvals(address);
          AdrVal = address;
          adr_val_flags = disp_eval_result.Flags;
          AdrMode = ModAbs;
          goto found;
        }
        else if (ModeMask & MModIOAbs)
        {
          if (!mFirstPassUnknownOrQuestionable(disp_eval_result.Flags) && !ChkRangeByType(disp_acc, UInt8, pArg))
            return AdrMode;
          ChkSpace(SegIO, disp_eval_result.AddrSpaceMask);
          AdrVals[0] = address & 0xff;
          AdrCnt = 1;
          AdrMode = ModIOAbs;
          goto found;
        }
        else
          goto inv_mode;
      }

      case 0:
        if ((z80_eval_cb_data.addr_reg_size != eSymbolSize16Bit) || disp_acc) /* no (B), (BC+d) */
          goto wrong;
        else /* (BC) */
        {
          AdrMode = ModIndReg16;
          AdrPart = 0;
          goto found;
        }

      case 1:
        if (z80_eval_cb_data.addr_reg_size == eSymbolSize16Bit) /* (DE) */
        {
          if (disp_acc)
            goto wrong;
          AdrMode = ModIndReg16;
          AdrPart = 1;
          goto found;
        }
        else /* (C), (FF00+C) on Sharp/GB */
        {
          if (!disp_acc || (disp_acc == 0xff00))
          {
            AdrMode = ModIndReg8;
            goto found;
          }
          else
            goto wrong;
        }

      case 2:
        if ((z80_eval_cb_data.addr_reg_size != eSymbolSize16Bit) || disp_acc) /* no (D), (HL+d) */
          goto wrong;
        else /* (HL) */
        {
          AdrMode = ModReg8; /* (HL) is M-Reg */
          AdrPart = 6;
          goto found;
        }

      case (IXPrefix & 0xf0) | 2: /* (IX+d) */
      case (IYPrefix & 0xf0) | 2: /* (IY+d) */
      case 3: /* (SP+d) */
        if (!mFirstPassUnknownOrQuestionable(disp_eval_result.Flags) && !ChkRangeByType(disp_acc, SInt8, pArg))
          return AdrMode;
        if (z80_eval_cb_data.addr_reg == 3)
          AdrMode = ModSPRel;
        else
        {
          AdrMode = ModReg8;
          AdrPart = 6;
          BAsmCode[PrefixCnt++] = 0x0d | (z80_eval_cb_data.addr_reg & 0xf0);
        }
        AdrVals[0] = disp_acc & 0xff;
        AdrCnt = 1;
        if ((disp_acc < -0x80l) || (disp_acc > 0x7fl))
        {
          AdrVals[AdrCnt++] = (disp_acc >> 8) & 0xff;
          if ((disp_acc >= -0x8000l) && (disp_acc <= 0x7fffl))
            ChangeDDPrefix(ePrefixIB);
          else
          {
            AdrVals[AdrCnt++] = (disp_acc >> 16) & 0xff;
            ChangeDDPrefix(ePrefixIW);
          }
        }
        goto found;

      wrong:
      default:
        WrStrErrorPos(ErrNum_InvAddrMode, pArg);
        return AdrMode;
    }
  }

  /* ...immediate */

  if (!(ModeMask & MModImm))
    goto inv_mode;
  switch (OpSize)
  {
    case 0xff:
      if (ModeMask & MModImm)
        WrError(ErrNum_UndefOpSizes);
      else
        AdrMode = ModImm;  /* will fail on test @ label found */
      break;
    case eSymbolSize8Bit:
      AdrVals[0] = EvalStrIntExpression(pArg, Int8, &OK);
      if (OK)
      {
        AdrMode = ModImm;
        AdrCnt = 1;
      }
      break;
    case eSymbolSize16Bit:
      if (InLongMode())
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
      else
      {
        AdrInt = EvalStrIntExpression(pArg, Int16, &OK);
        if (OK)
        {
          AdrVals[0] = Lo(AdrInt);
          AdrVals[1] = Hi(AdrInt);
          AdrMode = ModImm;
          AdrCnt = 2;
        }
      }
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

/*!------------------------------------------------------------------------
 * \fn     DecodeAdr_A(const tStrComp *p_arg)
 * \brief  check whether argument is accumulator (including possible register aliases)
 * \param  p_arg source argument
 * \return True if it is
 * ------------------------------------------------------------------------ */

static Boolean DecodeAdr_A(const tStrComp *p_arg)
{
  if (DecodeAdr(p_arg, MModReg8) != ModReg8)
    return False;
  if (AdrPart != AccReg)
  {
    WrStrErrorPos(ErrNum_InvAddrMode, p_arg);
    return False;
  }
  else
    return True;
}

/*!------------------------------------------------------------------------
 * \fn     DecodeAdr_HL(const tStrComp *p_arg)
 * \brief  check whether argument is HL (including possible register aliases)
 * \param  p_arg source argument
 * \return True if it is
 * ------------------------------------------------------------------------ */

static Boolean DecodeAdr_HL(const tStrComp *p_arg)
{
  if (DecodeAdr(p_arg, MModReg16) != ModReg16)
    return False;
  if ((AdrPart != HLReg) || (PrefixCnt > 0))
  {
    WrStrErrorPos(ErrNum_InvAddrMode, p_arg);
    return False;
  }
  else
    return True;
}

/*!------------------------------------------------------------------------
 * \fn     DecodeAdrWithF(const tStrComp *pArg, Boolean AllowF)
 * \brief  Handle address expression, treating F as 8th register
 * \param  pArg source argument
 * \param  allow 'F' at all?
 * ------------------------------------------------------------------------ */

static void DecodeAdrWithF(const tStrComp *pArg, Boolean AllowF)
{
    Boolean applies_to_cpu = 1;

  if (applies_to_cpu
   && AllowF
   && !as_strcasecmp(pArg->str.p_str, "F"))
  {
    AdrMode = ModReg8;
    AdrPart = 6;
    return;
  }

  DecodeAdr(pArg, MModAll);

  /* if 110 denotes F, it cannot denote (HL) */

  if (applies_to_cpu
   && (AdrMode == ModReg8)
   && (AdrPart == 6))
  {
    AdrMode = ModNone;
    WrStrErrorPos(ErrNum_InvAddrMode, pArg);
  }
}

static Boolean ImmIs8(void)
{
  Word tmp;

  if (AdrCnt < 2)
    return True;

  tmp = (Word) AdrVals[AdrCnt - 2];

  return ((tmp <= 255) || (tmp >= 0xff80));
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

static Boolean ParPair(const char *Name1, const char *Name2)
{
  return (((!as_strcasecmp(ArgStr[1].str.p_str, Name1)) && (!as_strcasecmp(ArgStr[2].str.p_str, Name2))) ||
          ((!as_strcasecmp(ArgStr[1].str.p_str, Name2)) && (!as_strcasecmp(ArgStr[2].str.p_str, Name1))));
}

/*!------------------------------------------------------------------------
 * \fn     has_index_prefix(void)
 * \brief  Check if HL has been replaced by IX/IY via prefix
 * \return True if yes
 * ------------------------------------------------------------------------ */

static Boolean has_index_prefix(void)
{
  return (PrefixCnt > 0)
      && ((BAsmCode[PrefixCnt - 1] == IXPrefix)
       || (BAsmCode[PrefixCnt - 1] == IYPrefix));
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

static void store_prefix(prefix_store_t *p_store, Byte old_prefix_cnt)
{
  p_store->cnt = PrefixCnt;
  p_store->present = p_store->cnt > old_prefix_cnt;
  p_store->value = p_store->present ? BAsmCode[p_store->cnt - 1] : 0x00;
}

/*!------------------------------------------------------------------------
 * \fn     remove_prefix(int byte_index)
 * \brief  remove a code prefix
 * \param  byte_index 0 -> remove last prefix
 *                    1 -> remove second last prefix
 * \return value of removed prefix
 * ------------------------------------------------------------------------ */

static Byte remove_prefix(int byte_index)
{
  Byte ret;

  if (PrefixCnt < (byte_index + 1))
    WrError(ErrNum_InternalError);
  ret = BAsmCode[PrefixCnt - 1 - byte_index];
  memmove(&BAsmCode[PrefixCnt - 1 - byte_index],
          &BAsmCode[PrefixCnt     - byte_index],
          byte_index);
  PrefixCnt--;
  return ret;
}

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
 * \fn     DecodeAcc(Word Index)
 * \brief  handle instructions with accumulator as argument
 * \param  Index * to instruction description
 * ------------------------------------------------------------------------ */

static void DecodeAcc(Word Index)
{
  BaseOrder *POrder = AccOrders + Index;

  if (!ChkArgCnt(0, 1)
   || !chk_core_mask(POrder->core_mask))
    return;

  if (ArgCnt && !DecodeAdr_A(&ArgStr[1]))
    return;

  append_to_prefixes(POrder->Code);
  CodeLen = PrefixCnt;
}

/*!------------------------------------------------------------------------
 * \fn     DecodeHL(Word Index)
 * \brief  handle instructions with HL as argument
 * \param  Index * to instruction description
 * ------------------------------------------------------------------------ */

static void DecodeHL(Word Index)
{
  BaseOrder *POrder = HLOrders + Index;

  if (!ChkArgCnt(0, 1)
   || !chk_core_mask(POrder->core_mask))
    return;

  if (ArgCnt && !DecodeAdr_HL(&ArgStr[1]))
    return;

  append_to_prefixes(POrder->Code);
  CodeLen = PrefixCnt;
}

/*!------------------------------------------------------------------------
 * \fn     DecodeLD(Word IsLDW)
 * \brief  handle LD(W) instruction
 * \param  IsLDW LD or LDW?
 * ------------------------------------------------------------------------ */

static void DecodeLD(Word IsLDW)
{
  Byte AdrByte,HLen;
  int z;
  Byte HVals[5];

  if (ChkArgCnt(2, 2))
  {
    unsigned dest_mask;

    dest_mask = MModReg8 | MModReg16 | MModIndReg16 | MModAbs | MModSPRel;
    dest_mask |= MModRef | MModInt;
    DecodeAdr(&ArgStr[1], dest_mask);
    switch (AdrMode)
    {
      case ModReg8:
        if (AdrPart == AccReg) /* LD A, ... */
        {
          unsigned src_mask;

          OpSize = eSymbolSize8Bit;
          src_mask = MModReg8 | MModReg16 | MModIndReg16 | MModImm | MModAbs | MModSPRel;
          src_mask |= MModRef | MModInt;
          DecodeAdr(&ArgStr[2], src_mask);
          switch (AdrMode)
          {
            case ModReg8: /* LD A, R8/RX8/(HL)/(XY+D) */
              BAsmCode[PrefixCnt] = 0x78 + AdrPart;
              memcpy(BAsmCode + PrefixCnt + 1, AdrVals, AdrCnt);
              CodeLen = PrefixCnt + 1 + AdrCnt;
              break;
            case ModIndReg8: /* LD A,(FF00+C) */
              BAsmCode[0] = 0xf2;
              CodeLen = 1;
              break;
            case ModHLInc: /* LD A,(HLI) */
              BAsmCode[0] = 0x2a;
              CodeLen = 1;
              break;
            case ModHLDec: /* LD A,(HLD) */
              BAsmCode[0] = 0x3a;
              CodeLen = 1;
              break;
            case ModIndReg16: /* LD A, (BC)/(DE) */
              BAsmCode[PrefixCnt++] = 0x0a + (AdrPart << 4);
              CodeLen = PrefixCnt;
              break;
            case ModImm: /* LD A, imm8 */
              BAsmCode[PrefixCnt++] = 0x3e;
              BAsmCode[PrefixCnt++] = AdrVals[0];
              CodeLen = PrefixCnt;
              break;
            case ModAbs: /* LD a, (adr) */
              BAsmCode[PrefixCnt] = 0x3a;
              memcpy(BAsmCode + PrefixCnt + 1, AdrVals, AdrCnt);
              CodeLen = PrefixCnt + 1 + AdrCnt;
              break;
            case ModRef: /* LD A, R */
              BAsmCode[PrefixCnt++] = 0xed;
              BAsmCode[PrefixCnt++] = 0x5f;
              CodeLen = PrefixCnt;
              break;
            case ModInt: /* LD A, I */
              BAsmCode[PrefixCnt++] = 0xed;
              BAsmCode[PrefixCnt++] = 0x57;
              CodeLen = PrefixCnt;
              break;
            case ModMB: /* LD A, MB */
              BAsmCode[PrefixCnt++] = 0xed;
              BAsmCode[PrefixCnt++] = 0x6e;
              CodeLen = PrefixCnt;
              break;
            default:
              if (AdrMode != ModNone) WrError(ErrNum_InvAddrMode);
          }
        }
        else if ((AdrPart != MReg) && !has_index_prefix()) /* LD R8, ... */
        {
          AdrByte = AdrPart; OpSize = 0; DecodeAdr(&ArgStr[2], MModReg8 | MModImm);
          switch (AdrMode)
          {
            case ModReg8: /* LD R8, R8/RX8/(HL)/(XY+D) */
              /* if (I(XY)+d) as source, cannot use H/L as target ! */
              if (((AdrByte == HReg) || (AdrByte == LReg)) && IndexPrefix() && (AdrCnt == 0)) WrError(ErrNum_InvAddrMode);
              else
              {
                BAsmCode[PrefixCnt] = 0x40 + (AdrByte << 3) + AdrPart;
                memcpy(BAsmCode + PrefixCnt + 1, AdrVals, AdrCnt);
                CodeLen = PrefixCnt + 1 + AdrCnt;
              }
              break;
            case ModImm: /* LD R8, imm8 */
              BAsmCode[0] = 0x06 + (AdrByte << 3); BAsmCode[1] = AdrVals[0];
              CodeLen = 2;
              break;
            default:
              if (AdrMode != ModNone) WrError(ErrNum_InvAddrMode);
          }
        }
        else if ((AdrPart == HReg) || (AdrPart == LReg)) /* LD RX8, ... */
        {
          AdrByte = AdrPart; OpSize = 0; DecodeAdr(&ArgStr[2], MModAll);
          switch (AdrMode)
          {
            case ModReg8: /* LD RX8, R8/RX8 */
              if (AdrPart == 6) WrError(ErrNum_InvAddrMode);        /* stopped here */
              else if ((AdrPart >= 4) && (AdrPart <= 5) && (PrefixCnt != 2)) WrError(ErrNum_InvAddrMode);
              else if ((AdrPart >= 4) && (AdrPart <= 5) && (BAsmCode[0] != BAsmCode[1])) WrError(ErrNum_InvAddrMode);
              else
              {
                if (PrefixCnt == 2) PrefixCnt--;
                BAsmCode[PrefixCnt] = 0x40 + (AdrByte << 3) + AdrPart;
                CodeLen = PrefixCnt + 1;
              }
              break;
            case ModImm: /* LD RX8,imm8 */
              BAsmCode[PrefixCnt]=0x06+(AdrByte << 3);
              BAsmCode[PrefixCnt+1]=AdrVals[0];
              CodeLen=PrefixCnt+2;
              break;
            default:
              if (AdrMode!=ModNone) WrError(ErrNum_InvAddrMode);
          }
        }
        else /* LD (HL)/(XY+d),... */
        {
          HLen = AdrCnt;
          memcpy(HVals, AdrVals, AdrCnt);
          z = PrefixCnt;
          if ((z == 0) && (IsLDW))
          {
            OpSize = 1;
            MayLW = True;
          }
          else
            OpSize = 0;
          DecodeAdr(&ArgStr[2], MModAll);
          switch (AdrMode)
          {
            case ModReg8: /* LD (HL)/(XY+D),R8 */
              if ((PrefixCnt != z) || (AdrPart == 6)) WrError(ErrNum_InvAddrMode);
              else
              {
                BAsmCode[PrefixCnt] = 0x70 + AdrPart;
                memcpy(BAsmCode + PrefixCnt + 1, HVals, HLen);
                CodeLen=PrefixCnt + 1 + HLen;
              }
              break;
            case ModImm: /* LD (HL)/(XY+D),imm8:16:32 */
              if ((z == 0) && (IsLDW))
              {
                BAsmCode[PrefixCnt] = 0xed;
                BAsmCode[PrefixCnt + 1] = 0x36;
                memcpy(BAsmCode + PrefixCnt + 2, AdrVals, AdrCnt);
                CodeLen = PrefixCnt + 2 + AdrCnt;
              }
              else
              {
                BAsmCode[PrefixCnt] = 0x36;
                memcpy(BAsmCode + 1 + PrefixCnt, HVals, HLen);
                BAsmCode[PrefixCnt + 1 + HLen] = AdrVals[0];
                CodeLen = PrefixCnt + 1 + HLen + AdrCnt;
              }
              break;
            case ModReg16: /* LD (HL)/(XY+D),R16/XY */
              if (AdrPart == SPReg) WrError(ErrNum_InvAddrMode);
              else if (HLen == 0)
              {
                if (PrefixCnt == z) /* LD (HL),R16 */
                {
                  if (AdrPart == 2)
                    AdrPart = 3;
                  BAsmCode[PrefixCnt] = 0xfd;
                  BAsmCode[PrefixCnt + 1] = 0x0f + (AdrPart << 4);
                  CodeLen = PrefixCnt + 2;
                }
                else /* LD (HL),XY */
                {
                  CodeLen = PrefixCnt + 1;
                  BAsmCode[PrefixCnt] = 0x31;
                  CodeLen = 1 + PrefixCnt;
                }
              }
              else
              {
                if (PrefixCnt == z) /* LD (XY+D),R16 */
                {
                  if (AdrPart == HLReg)
                    AdrPart = 3;
                  BAsmCode[PrefixCnt] = 0xcb;
                  memcpy(BAsmCode + PrefixCnt + 1, HVals, HLen);
                  BAsmCode[PrefixCnt + 1 + HLen] = 0x0b + (AdrPart << 4);
                  CodeLen = PrefixCnt + 1 + HLen + 1;
                }
                else /* LD (XY+D), IX/Y */
                {
                  if (BAsmCode[0] == BAsmCode[1]) WrError(ErrNum_InvAddrMode);
                  else
                  {
                    (void)remove_prefix(0);
                    BAsmCode[PrefixCnt] = 0xcb;
                    memcpy(BAsmCode + PrefixCnt + 1, HVals, HLen);
                    BAsmCode[PrefixCnt + 1 + HLen] = 0x2b;
                    CodeLen = PrefixCnt + 1 + HLen + 1;
                  }
                }
              }
              break;
            default:
              if (AdrMode!=ModNone) WrError(ErrNum_InvAddrMode);
          }
        }
        break;
      case ModReg16:
        if (AdrPart == SPReg) /* LD SP,... */
        {
          OpSize = 1;
          MayLW = True;
          DecodeAdr(&ArgStr[2], MModAll);
          switch (AdrMode)
          {
            case ModReg16: /* LD SP,HL/XY */
              if (AdrPart != 2) WrError(ErrNum_InvAddrMode);
              else
              {
                BAsmCode[PrefixCnt] = 0xf9;
                CodeLen = PrefixCnt + 1;
              }
              break;
            case ModImm: /* LD SP,imm16:32 */
              BAsmCode[PrefixCnt] = 0x31;
              memcpy(BAsmCode + PrefixCnt + 1, AdrVals, AdrCnt);
              CodeLen = PrefixCnt + 1 + AdrCnt;
              break;
            case ModAbs: /* LD SP,(adr) */
              BAsmCode[PrefixCnt] = 0xed;
              BAsmCode[PrefixCnt + 1] = 0x7b;
              memcpy(BAsmCode + PrefixCnt + 2, AdrVals, AdrCnt);
              CodeLen = PrefixCnt + 2 + AdrCnt;
              break;
            default:
              if (AdrMode != ModNone) WrError(ErrNum_InvAddrMode);
          }
        }
        else if (!has_index_prefix()) /* LD R16,... */
        {
          unsigned ModeMask = MModAll;
          unsigned after_dest_prefixcnt = PrefixCnt;

          AdrByte = (AdrPart == 2) ? 3 : AdrPart;
          OpSize = 1;
          MayLW = True;
          DecodeAdr(&ArgStr[2], ModeMask);
          switch (AdrMode)
          {
            case ModInt: /* LD HL,I */
              if (AdrByte != 3) WrError(ErrNum_InvAddrMode);
              else
              {
                BAsmCode[0] = 0xdd;
                BAsmCode[1] = 0x57;
                CodeLen = 2;
              }
              break;
            case ModReg8:
              if (AdrPart != 6) WrError(ErrNum_InvAddrMode);
              else if (PrefixCnt == after_dest_prefixcnt) /* LD R16,(HL) */
              {
                BAsmCode[0] = 0xdd;
                BAsmCode[1] = 0x0f + (AdrByte << 4);
                CodeLen = 2;
              }
              else /* LD R16,(XY+d) */
              {
                BAsmCode[PrefixCnt] = 0xcb;
                memcpy(BAsmCode+PrefixCnt + 1, AdrVals, AdrCnt);
                BAsmCode[PrefixCnt + 1 + AdrCnt] = 0x03 + (AdrByte << 4);
                CodeLen = PrefixCnt + 1 + AdrCnt + 1;
              }
              break;
            case ModReg16:
              if (AdrPart == 3) WrError(ErrNum_InvAddrMode);
              else if (PrefixCnt == 0) /* LD R16,R16 */
              {
                if (AdrPart == 2)
                  AdrPart = 3;
                else if (AdrPart == 0)
                  AdrPart = 2;
                BAsmCode[0] = 0xcd + (AdrPart << 4);
                BAsmCode[1] = 0x02 + (AdrByte << 4);
                CodeLen = 2;
              }
              else /* LD R16,XY */
              {
                BAsmCode[PrefixCnt] = 0x0b + (AdrByte << 4);
                CodeLen=PrefixCnt + 1;
              }
              break;
            case ModIndReg16: /* LD R16,(R16) */
              CodeLen = 2;
              BAsmCode[0] = 0xdd;
              BAsmCode[1] = 0x0c + (AdrByte << 4) + AdrPart;
              break;
            case ModImm: /* LD R16,imm */
              if (AdrByte == 3)
                AdrByte = 2;
              CodeLen=PrefixCnt + 1 + AdrCnt;
              BAsmCode[PrefixCnt] = 0x01 + (AdrByte << 4);
              memcpy(BAsmCode + PrefixCnt + 1, AdrVals, AdrCnt);
              break;
            case ModAbs: /* LD R16,(adr) */
              if (AdrByte == 3)
              {
                BAsmCode[PrefixCnt] = 0x2a;
                memcpy(BAsmCode + PrefixCnt + 1, AdrVals, AdrCnt);
                CodeLen = 1 + PrefixCnt + AdrCnt;
              }
              else
              {
                BAsmCode[PrefixCnt] = 0xed;
                BAsmCode[PrefixCnt+1] = 0x4b + (AdrByte << 4);
                memcpy(BAsmCode + PrefixCnt + 2, AdrVals, AdrCnt);
                CodeLen = PrefixCnt + 2 + AdrCnt;
              }
              break;
            case ModSPAdd:
              BAsmCode[0] = 0xf8;
              BAsmCode[1] = AdrVals[0];
              CodeLen = 2;
              break;
            case ModSPRel: /* LD R16,(SP+D) */
              BAsmCode[PrefixCnt] = 0xdd;
              BAsmCode[PrefixCnt + 1] = 0xcb;
              memcpy(BAsmCode + PrefixCnt + 2, AdrVals, AdrCnt);
              BAsmCode[PrefixCnt + 2 + AdrCnt] = 0x01 + (AdrByte << 4);
              CodeLen=PrefixCnt + 3 + AdrCnt;
              break;
            default:
              if (AdrMode != ModNone) WrError(ErrNum_InvAddrMode);
          }
        }
        else /* LD XY,... */
        {
          OpSize = 1;
          MayLW = True;
          DecodeAdr(&ArgStr[2], MModAll);
          switch (AdrMode)
          {
            case ModReg8:
              if (AdrPart != 6) WrError(ErrNum_InvAddrMode);
              else if (AdrCnt == 0) /* LD XY,(HL) */
              {
                BAsmCode[PrefixCnt] = 0x33;
                CodeLen = PrefixCnt + 1;
              }
              else /* LD XY,(XY+D) */
              {
                if (BAsmCode[0] == BAsmCode[1]) WrError(ErrNum_InvAddrMode);
                else
                {
                  (void)remove_prefix(1);
                  BAsmCode[PrefixCnt] = 0xcb;
                  memcpy(BAsmCode + PrefixCnt + 1, AdrVals, AdrCnt);
                  BAsmCode[PrefixCnt + 1 + AdrCnt] = 0x23;
                  CodeLen = PrefixCnt + 1 + AdrCnt + 1;
                }
              }
              break;
            case ModReg16:
              if (AdrPart == 3) WrError(ErrNum_InvAddrMode);
              else if (PrefixCnt == 1) /* LD XY,R16 */
              {
                if (AdrPart == 2) AdrPart = 3;
                CodeLen = 1 + PrefixCnt;
                BAsmCode[PrefixCnt] = 0x07 + (AdrPart << 4);
              }
              else if (BAsmCode[0] == BAsmCode[1]) WrError(ErrNum_InvAddrMode);
              else /* LD XY,XY */
              {
                BAsmCode[--PrefixCnt] = 0x27;
                CodeLen = 1 + PrefixCnt;
              }
              break;
            case ModIndReg16:
              BAsmCode[PrefixCnt] = 0x03 + (AdrPart << 4);
              CodeLen = PrefixCnt + 1;
              break;
            case ModImm: /* LD XY,imm16:32 */
              BAsmCode[PrefixCnt] = 0x21;
              memcpy(BAsmCode+PrefixCnt + 1, AdrVals, AdrCnt);
              CodeLen = PrefixCnt + 1 + AdrCnt;
              break;
            case ModAbs: /* LD XY,(adr) */
              BAsmCode[PrefixCnt] = 0x2a;
              memcpy(BAsmCode + PrefixCnt + 1, AdrVals, AdrCnt);
              CodeLen = PrefixCnt + 1 + AdrCnt;
              break;
            case ModSPRel: /* LD XY,(SP+D) */
              BAsmCode[PrefixCnt] = 0xcb;
              memcpy(BAsmCode + PrefixCnt + 1, AdrVals, AdrCnt);
              BAsmCode[PrefixCnt + 1 + AdrCnt] = 0x21;
              CodeLen = PrefixCnt + 1 + AdrCnt + 1;
              break;
            default:
              if (AdrMode!=ModNone) WrError(ErrNum_InvAddrMode);
          }
        }
        break;
      case ModIndReg8:
        DecodeAdr(&ArgStr[2], MModReg8);
        switch (AdrMode)
        {
          case ModReg8:
            if (AdrPart != AccReg) WrStrErrorPos(ErrNum_InvAddrMode, &ArgStr[2]);
            else
            {
              BAsmCode[0] = 0xe2;
              CodeLen = 1;
            }
            break;
          default:
            break;
        }
        break;
      case ModHLInc:
        DecodeAdr(&ArgStr[2], MModReg8);
        switch (AdrMode)
        {
          case ModReg8:
            if (AdrPart != AccReg) WrStrErrorPos(ErrNum_InvAddrMode, &ArgStr[2]);
            else
            {
              BAsmCode[0] = 0x22;
              CodeLen = 1;
            }
            break;
          default:
            break;
        }
        break;
      case ModHLDec:
        DecodeAdr(&ArgStr[2], MModReg8);
        switch (AdrMode)
        {
          case ModReg8:
            if (AdrPart != AccReg) WrStrErrorPos(ErrNum_InvAddrMode, &ArgStr[2]);
            else
            {
              BAsmCode[0] = 0x32;
              CodeLen = 1;
            }
            break;
          default:
            break;
        }
        break;
      case ModIndReg16:
        AdrByte = AdrPart;
        if (IsLDW)
        {
          OpSize = 1;
          MayLW = True;
        }
        else
          OpSize = 0;
        DecodeAdr(&ArgStr[2], MModAll);
        switch (AdrMode)
        {
          case ModReg8: /* LD (R16),A */
            if (AdrPart != AccReg) WrStrErrorPos(ErrNum_InvAddrMode, &ArgStr[2]);
            else
            {
              CodeLen = PrefixCnt + 1;
              BAsmCode[PrefixCnt] = 0x02 + (AdrByte << 4);
            }
            break;
          case ModReg16:
            if (AdrPart == 3) WrError(ErrNum_InvAddrMode);
            else if (PrefixCnt == 0) /* LD (R16),R16 */
            {
              if (AdrPart == 2)
                AdrPart = 3;
              BAsmCode[0] = 0xfd;
              BAsmCode[1] = 0x0c + AdrByte + (AdrPart << 4);
              CodeLen = 2;
            }
            else /* LD (R16),XY */
            {
              CodeLen = PrefixCnt;
              BAsmCode[CodeLen++] = 0x01 + (AdrByte << 4);
            }
            break;
          case ModImm:
            if (!IsLDW) WrError(ErrNum_InvAddrMode);
            else
            {
              CodeLen = PrefixCnt;
              BAsmCode[CodeLen++] = 0xed;
              BAsmCode[CodeLen++] = 0x06 + (AdrByte << 4);
              AppendAdrVals();
            }
            break;
          default:
            if (AdrMode != ModNone) WrError(ErrNum_InvAddrMode);
        }
        break;
      case ModAbs:
        HLen = AdrCnt;
        memcpy(HVals, AdrVals, AdrCnt);
        OpSize = 0;
        DecodeAdr(&ArgStr[2], MModReg8 | MModReg16);
        switch (AdrMode)
        {
          case ModReg8: /* LD (adr),A */
            if (AdrPart != AccReg) WrStrErrorPos(ErrNum_InvAddrMode, &ArgStr[2]);
            else
            {
              CodeLen = PrefixCnt;
              BAsmCode[CodeLen++] = 0x32;
              AppendVals(HVals, HLen);
            }
            break;
          case ModReg16:
            if (AdrPart == 2) /* LD (adr),HL/XY */
            {
              CodeLen = PrefixCnt;
              BAsmCode[CodeLen++] = 0x22;
              AppendVals(HVals, HLen);
            }
            else /* LD (adr),R16 */
            {
              CodeLen = PrefixCnt;
              BAsmCode[CodeLen++] = 0xed;
              BAsmCode[CodeLen++] = 0x43 + (AdrPart << 4);
              AppendVals(HVals, HLen);
            }
            break;
          default:
            if (AdrMode!=ModNone) WrError(ErrNum_InvAddrMode);
        }
        break;
      case ModInt:
        switch (DecodeAdr(&ArgStr[2], MModReg8 | MModReg16))
        {
          case ModReg8: /* LD I,A */
            if (AdrPart != AccReg) WrStrErrorPos(ErrNum_InvAddrMode, &ArgStr[2]);
            else
            {
              CodeLen = 2;
              BAsmCode[0] = 0xed;
              BAsmCode[1] = 0x47;
            }
            break;
          case ModReg16: /* LD I,HL */
            if ((AdrPart != HLReg) || PrefixCnt) WrStrErrorPos(ErrNum_InvAddrMode, &ArgStr[2]);
            else
            {
              CodeLen = 2;
              BAsmCode[0] = 0xdd;
              BAsmCode[1] = 0x47;
            }
            break;
          default:
            break;
        }
        break;
      case ModRef:
        if (DecodeAdr_A(&ArgStr[2])) /* LD R,A */
        {
          CodeLen = 2;
          BAsmCode[0] = 0xed;
          BAsmCode[1] = 0x4f;
        }
        else WrError(ErrNum_InvAddrMode);
        break;
      case ModMB:
        if (DecodeAdr_A(&ArgStr[2])) /* LD MB,A */
        {
          CodeLen = 2;
          BAsmCode[0] = 0xed;
          BAsmCode[1] = 0x6d;
        }
        else WrError(ErrNum_InvAddrMode);
        break;
      case ModSPRel:
        {
          HLen = AdrCnt;
          memcpy(HVals, AdrVals, AdrCnt);
          OpSize = 0;
          DecodeAdr(&ArgStr[2], MModAll);
          switch (AdrMode)
          {
            case ModReg16:
              if (AdrPart == 3) WrError(ErrNum_InvAddrMode);
              else if (PrefixCnt == 0) /* LD (SP+D),R16 */
              {
                if (AdrPart == 2)
                  AdrPart = 3;
                CodeLen = PrefixCnt;
                BAsmCode[CodeLen++] = 0xdd;
                BAsmCode[CodeLen++] = 0xcb;
                AppendVals(HVals, HLen);
                BAsmCode[CodeLen++] = 0x09 + (AdrPart << 4);
              }
              else /* LD (SP+D),XY */
              {
                CodeLen = PrefixCnt;
                BAsmCode[CodeLen++] = 0xcb;
                AppendVals(HVals, HLen);
                BAsmCode[CodeLen++] = 0x29;
              }
              break;
            default:
              if (AdrMode!=ModNone) WrError(ErrNum_InvAddrMode);
          }
        }
        break;
      default:
        if (AdrMode != ModNone) WrError(ErrNum_InvAddrMode);
    }  /* outer switch */
  }
}

/*!------------------------------------------------------------------------
 * \fn     DecodeLDX(Word Code)
 * \brief  decode LDX instruction
 * ------------------------------------------------------------------------ */

static void DecodeLDX(Word Code)
{
  UNUSED(Code);

  if (!ChkArgCnt(2, 2))
    return;
  DecodeAdr(&ArgStr[1], MModReg8 | MModAbs);
  switch (AdrMode)
  {
    case ModReg8:
      if (AdrPart != AccReg) WrStrErrorPos(ErrNum_InvReg, &ArgStr[1]);
      else
      {
        DecodeAdr(&ArgStr[2], MModAbs);
        if (AdrMode == ModAbs)
        {
          BAsmCode[0] = 0xfa;
          memcpy(BAsmCode + 1, AdrVals, AdrCnt);
          CodeLen = 1 + AdrCnt;
        }
      }
      break;
    case ModAbs:
      memcpy(BAsmCode + 1, AdrVals, AdrCnt);
      DecodeAdr(&ArgStr[2], MModReg8);
      if (AdrMode == ModReg8)
      {
        if (AdrPart != AccReg) WrStrErrorPos(ErrNum_InvReg, &ArgStr[2]);
        else
        {
          BAsmCode[0] = 0xea;
          CodeLen = 3;
        }
      }
      break;
    default:
      break;
  }
}

static void DecodeALU8(Word Code)
{
  switch (ArgCnt)
  {
    case 1:
      AdrMode = ModReg8;
      AdrPart = AccReg;
      AdrCnt = 0;
      break;
    case 2:
      DecodeAdr(&ArgStr[1], MModReg8 | MModReg16);
      break;
    default:
      (void)ChkArgCnt(1, 2);
      return;
  }

  switch (AdrMode)
  {
    case ModReg16:
      if (Code != 2) WrStrErrorPos(ErrNum_InvAddrMode, &ArgStr[1]);
      else if (AdrPart == HLReg)
      {
        OpSize = 1;
        if (DecodeAdr(&ArgStr[ArgCnt], MModAbs) == ModAbs)
        {
          CodeLen = PrefixCnt;
          BAsmCode[CodeLen++] = 0xed;
          BAsmCode[CodeLen++] = 0xd6;
          AppendAdrVals();
        }
      }
      else if (AdrPart == SPReg)
      {
        OpSize = 1;
        if (DecodeAdr(&ArgStr[ArgCnt], MModImm) == ModImm)
        {
          CodeLen = 0;
          BAsmCode[CodeLen++] = 0xed;
          BAsmCode[CodeLen++] = 0x92;
          AppendAdrVals();
          break;
        }
      }
      else
        WrStrErrorPos(ErrNum_InvAddrMode, &ArgStr[1]);
      break;
    case ModReg8:
      if (AdrPart != AccReg) WrStrErrorPos(ErrNum_InvAddrMode, &ArgStr[1]);
      else
      {
        OpSize = 0;
        switch (DecodeAdr(&ArgStr[ArgCnt], MModReg8 | MModImm))
        {
          case ModReg8:
            CodeLen = PrefixCnt + 1 + AdrCnt;
            BAsmCode[PrefixCnt] = 0x80 + (Code << 3) + AdrPart;
            memcpy(BAsmCode + PrefixCnt + 1, AdrVals, AdrCnt);
            break;
          case ModImm:
            if (!ImmIs8()) WrStrErrorPos(ErrNum_OverRange, &ArgStr[ArgCnt]);
            else
            {
              CodeLen = 2;
              BAsmCode[0] = 0xc6 + (Code << 3);
              BAsmCode[1] = AdrVals[0];
            }
            break;
          default:
            break;
        }
        break;
      default:
        break;
    }
  }
}

static void DecodeALU16(Word Code)
{
  if (ChkArgCnt(1, 2)
   && ((ArgCnt == 1) || DecodeAdr_HL(&ArgStr[1])))
  {
    OpSize = 1; DecodeAdr(&ArgStr[ArgCnt], MModAll);
    switch (AdrMode)
    {
      case ModReg16:
        if (PrefixCnt > 0)      /* wenn Register, dann nie DDIR! */
        {
          BAsmCode[PrefixCnt] = 0x87 + (Code << 3);
          CodeLen = 1 + PrefixCnt;
        }
        else if (AdrPart == 3) WrError(ErrNum_InvAddrMode);
        else
        {
          if (AdrPart == 2)
            AdrPart = 3;
          BAsmCode[0] = 0xed;
          BAsmCode[1] = 0x84 + (Code << 3) + AdrPart;
          CodeLen = 2;
        }
        break;
      case ModReg8:
        if ((AdrPart != 6) || (AdrCnt == 0)) WrError(ErrNum_InvAddrMode);
        else
        {
          CodeLen = PrefixCnt;
          BAsmCode[CodeLen++] = 0xc6 + (Code << 3);
          AppendAdrVals();
        }
        break;
      case ModImm:
        CodeLen = 0;
        BAsmCode[CodeLen++] = 0xed;
        BAsmCode[CodeLen++] = 0x86 + (Code << 3);
        AppendAdrVals();
        break;
      default:
        if (AdrMode != ModNone) WrError(ErrNum_InvAddrMode);
    }
  }
}

static void DecodeADD(Word Index)
{
  UNUSED(Index);

  if (ChkArgCnt(2, 2))
  {
    Byte raw_prefix_cnt = PrefixCnt;

    DecodeAdr(&ArgStr[1], MModNoImm);
    switch (AdrMode)
    {
      case ModReg8:
        if (AdrPart != AccReg) WrStrErrorPos(ErrNum_InvAddrMode, &ArgStr[1]);
        else
        {
          OpSize = 0; DecodeAdr(&ArgStr[2], MModReg8 | MModImm);
          switch (AdrMode)
          {
            case ModReg8:
              CodeLen = PrefixCnt;
              BAsmCode[CodeLen++] = 0x80 + AdrPart;
              AppendAdrVals();
              break;
            case ModImm:
              CodeLen = PrefixCnt;
              BAsmCode[CodeLen++] = 0xc6;
              AppendAdrVals();
              break;
            default:
              if (AdrMode != ModNone) WrError(ErrNum_InvAddrMode);
          }
        }
        break;
      case ModReg16:
        if (AdrPart == 3) /* SP */
        {
          OpSize = 1;
          DecodeAdr(&ArgStr[2], MModAll);
          switch (AdrMode)
          {
            case ModImm:
              BAsmCode[0] = 0xed; BAsmCode[1] = 0x82;
              memcpy(BAsmCode + 2, AdrVals, AdrCnt);
              CodeLen = 2 + AdrCnt;
              break;
            default:
              if (AdrMode != ModNone) WrError(ErrNum_InvAddrMode);
          }
        }
        else if (AdrPart != 2) WrError(ErrNum_InvAddrMode);
        else
        {
          prefix_store_t dest_prefix;
          store_prefix(&dest_prefix, raw_prefix_cnt);

          OpSize = 1; DecodeAdr(&ArgStr[2], MModAll);
          switch (AdrMode)
          {
            case ModReg16:
            {
              prefix_store_t src_prefix;
              store_prefix(&src_prefix, dest_prefix.cnt);

              if ((AdrPart == 2) && ((dest_prefix.present != src_prefix.present) || (dest_prefix.value != src_prefix.value))) WrError(ErrNum_InvAddrMode);
              else
              {
                if (dest_prefix.present && src_prefix.present)
                  PrefixCnt--;
                CodeLen = PrefixCnt;
                BAsmCode[CodeLen++] = 0x09 + (AdrPart << 4);
              }
              break;
            }
            case ModAbs:
              if (dest_prefix.present) WrError(ErrNum_InvAddrMode);
              else
              {
                CodeLen = PrefixCnt;
                BAsmCode[CodeLen++] = 0xed;
                BAsmCode[CodeLen++] = 0xc2;
                AppendAdrVals();
              }
              break;
            default:
              if (AdrMode!=ModNone) WrError(ErrNum_InvAddrMode);
          }
        }
        break;
      default:
        if (AdrMode != ModNone) WrError(ErrNum_InvAddrMode);
    }
  }
}

static void DecodeINC_DEC(Word Index)
{
  Word IsDEC = (Index & 1), IsWord = (Index & 2);

  if (ChkArgCnt(1, 1))
  {
    DecodeAdr(&ArgStr[1], MModReg8 | MModReg16);
    switch (AdrMode)
    {
      case ModReg8:
        if (IsWord) WrError(ErrNum_InvAddrMode);
        else
        {
          CodeLen = PrefixCnt;
          BAsmCode[CodeLen++] = 0x04 + (AdrPart << 3) + IsDEC;
          AppendAdrVals();
        }
        break;
      case ModReg16:
        CodeLen = PrefixCnt;
        BAsmCode[CodeLen++] = 0x03 + (AdrPart << 4) + (IsDEC << 3);
        break;
      default:
        if (AdrMode != ModNone) WrError(ErrNum_InvAddrMode);
    }
  }
}

static void DecodeShift8(Word Code)
{
  Byte reg_num = 0;
  int mem_arg_index;

  if (!ChkArgCnt(1, 1))
    return;

  /* dual arg (Z80 undoc): which is the extra destination register? This must be a 'simple' register (A,B,C,D,E,H,L): */

  if (ArgCnt >= 2)
  {
    if (DecodeReg8Core(ArgStr[1].str.p_str, &reg_num) && !(reg_num & 0xc0))
      mem_arg_index = 2;
    else if (DecodeReg8Core(ArgStr[2].str.p_str, &reg_num) && !(reg_num & 0xc0))
      mem_arg_index = 1;
    else
    {
      WrStrErrorPos(ErrNum_InvReg, &ArgStr[1]);
      return;
    }
  }

  /* single arg (documented version): */

  else
    mem_arg_index = 1;

  /* now decode the 'official argument': */

  OpSize = 0;
  DecodeAdr(&ArgStr[mem_arg_index], MModReg8);
  if (AdrMode != ModReg8)
    return;

  /* forbid IXL..IYU: */

  if ((PrefixCnt > 0) && (AdrPart != 6))
  {
    WrStrErrorPos(ErrNum_InvAddrMode, &ArgStr[mem_arg_index]);
    return;
  }

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

static void DecodeShift16(Word Code)
{
  if (!ChkArgCnt(1, 1));
  else
  {
    OpSize = 1; DecodeAdr(&ArgStr[1], MModNoImm);
    switch (AdrMode)
    {
      case ModReg16:
        if (PrefixCnt > 0)
        {
          BAsmCode[2] = 0x04 + (Code << 3) + ((BAsmCode[0] >> 5) & 1);
          BAsmCode[0] = 0xed;
          BAsmCode[1] = 0xcb;
          CodeLen = 3;
        }
        else if (AdrPart == 3) WrError(ErrNum_InvAddrMode);
        else
        {
          if (AdrPart == 2)
            AdrPart = 3;
          BAsmCode[0] = 0xed;
          BAsmCode[1] = 0xcb;
          BAsmCode[2] = (Code << 3) + AdrPart;
          CodeLen = 3;
        }
        break;
      case ModReg8:
        if (AdrPart != 6) WrError(ErrNum_InvAddrMode);
        else
        {
          if (AdrCnt == 0)
          {
            BAsmCode[0] = 0xed;
            PrefixCnt = 1;
          }
          CodeLen = PrefixCnt;
          BAsmCode[CodeLen++] = 0xcb;
          AppendAdrVals();
          BAsmCode[CodeLen++] = 0x02 + (Code << 3);
        }
        break;
      default:
        if (AdrMode != ModNone) WrError(ErrNum_InvAddrMode);
    }
  }
}

static void DecodeBit(Word Code)
{
  Byte reg_num = 0;
  int mem_arg_index, bit_arg_index;
  Boolean ok;

  /* extra undocumented dest register is not allowed for BIT */

  if (!ChkArgCnt(1, (Code != 0) ? 3 : 2))
    return;

  /* triple arg (Z80 undoc): which is the extra destination register? This must be a 'simple' register (A,B,C,D,E,H,L): */

  if (ArgCnt >= 3)
  {
    if (DecodeReg8Core(ArgStr[1].str.p_str, &reg_num) && !(reg_num & 0xc0))
    {
      mem_arg_index = 3;
      bit_arg_index = 2;
    }
    else if (DecodeReg8Core(ArgStr[3].str.p_str, &reg_num) && !(reg_num & 0xc0))
    {
      mem_arg_index = 2;
      bit_arg_index = 1;
    }
    else
    {
      WrStrErrorPos(ErrNum_InvReg, &ArgStr[1]);
      return;
    }
  }

  /* single arg (documented version): */

  else
  {
    mem_arg_index = 2;
    bit_arg_index = 1;
  }

  /* now decode the 'official arguments': */

  OpSize = 0;
  DecodeAdr(&ArgStr[mem_arg_index], MModReg8);
  if (AdrMode != ModReg8)
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

static void DecodeMLT(Word Index)
{
  UNUSED(Index);

  if (!ChkArgCnt(1, 1));
  else
  {
    DecodeAdr(&ArgStr[1], MModAll);
    if ((AdrMode != ModReg16) || has_index_prefix()) WrError(ErrNum_InvAddrMode);
    else
    {
      BAsmCode[PrefixCnt] = 0xed;
      BAsmCode[PrefixCnt + 1] = 0x4c + (AdrPart << 4);
      CodeLen = PrefixCnt + 2;
    }
  }
}

static void DecodeMULT_DIV(Word Code)
{
  const tStrComp *pSrcArg;

  if (2 == ArgCnt)
  {
    if (!DecodeAdr_HL(&ArgStr[1]))
      return;
  }

  OpSize = 1;
  pSrcArg = &ArgStr[ArgCnt];
  switch (DecodeAdr(pSrcArg, MModReg8 | MModReg16 | MModImm))
  {
    case ModReg8:
      if ((AdrPart != 6) || (PrefixCnt == 0)) WrStrErrorPos(ErrNum_InvAddrMode, pSrcArg);
      else
      {
        CodeLen = PrefixCnt;
        BAsmCode[CodeLen++] = 0xcb;
        AppendAdrVals();
        BAsmCode[CodeLen++] = 0x92 | Code;
      }
      break;
    case ModReg16:
      if (AdrPart == SPReg) WrStrErrorPos(ErrNum_InvAddrMode, pSrcArg);
      else if (PrefixCnt == 0)
      {
        if (AdrPart == 2)
          AdrPart = 3;
        BAsmCode[0] = 0xed;
        BAsmCode[1] = 0xcb;
        BAsmCode[2] = 0x90 + AdrPart + Code;
        CodeLen = 3;
      }
      else
      {
        BAsmCode[2] = 0x94 + ((BAsmCode[0] >> 5) & 1) + Code;
        BAsmCode[0] = 0xed;
        BAsmCode[1] = 0xcb;
        CodeLen = 3;
      }
      break;
    case ModImm:
      CodeLen = 0;
      BAsmCode[CodeLen++] = 0xed;
      BAsmCode[CodeLen++] = 0xcb;
      BAsmCode[CodeLen++] = 0x97 + Code;
      AppendAdrVals();
      break;
    default:
      break;
  }
}

static void DecodeTST(Word Index)
{
  UNUSED(Index);

  if (!ChkArgCnt(1, 2));
  else
  {
    if (ArgCnt == 2)
    {
      if (DecodeAdr(&ArgStr[1], MModReg8) != ModReg8)
        return;
      if (AdrPart != AccReg)
      {
        WrStrErrorPos(ErrNum_InvAddrMode, &ArgStr[1]);
        return;
      }
    }
    OpSize = eSymbolSize8Bit;
    DecodeAdr(&ArgStr[ArgCnt], MModAll);
    switch (AdrMode)
    {
      case ModReg8:
        if (has_index_prefix()) WrError(ErrNum_InvAddrMode);
        else
        {
          BAsmCode[PrefixCnt] = 0xed;
          BAsmCode[PrefixCnt + 1] = 4 + (AdrPart << 3);
          CodeLen = PrefixCnt + 2;
        }
        break;
      case ModImm:
        BAsmCode[PrefixCnt] = 0xed;
        BAsmCode[PrefixCnt + 1] = 0x64;
        BAsmCode[PrefixCnt + 2] = AdrVals[0];
        CodeLen = PrefixCnt + 3;
        break;
      default:
        if (AdrMode != ModNone) WrError(ErrNum_InvAddrMode);
    }
  }
}

/*!------------------------------------------------------------------------
 * \fn     DecodePUSH_POP(Word Code)
 * \brief  handle PUSH/POP instructions
 * \param  Code machine code (4 = PUSH family, 0 = POP family)
 * ------------------------------------------------------------------------ */

static void DecodePUSH_POP(Word Code)
{
  if (!ChkArgCnt(1, 1));
  else if (!as_strcasecmp(ArgStr[1].str.p_str, "SR"))
  {
    CodeLen = 2;
    BAsmCode[0] = 0xed;
    BAsmCode[1] = 0xc1 + Code;
  }
  else
  {
    OpSize = 1; MayLW = True;
    if (!as_strcasecmp(ArgStr[1].str.p_str, "AF"))
    {
      AdrPart = SPReg;
      AdrMode = ModReg16;
    }
    else
      DecodeAdr(&ArgStr[1], MModReg16 | ((Code == 4) ? MModImm : 0));
    switch (AdrMode)
    {
      case ModReg16:
        CodeLen = 1 + PrefixCnt;
        BAsmCode[PrefixCnt] = 0xc1 + (AdrPart << 4) + Code;
        break;
      case ModImm:
        CodeLen = PrefixCnt;
        BAsmCode[CodeLen++] = 0xfd;
        BAsmCode[CodeLen++] = 0xf5;
        AppendAdrVals();
        break;
      default:
        if (AdrMode != ModNone) WrError(ErrNum_InvAddrMode);
    }
  }
}

static void DecodeEX(Word Index)
{
  Boolean OK;
  Byte AdrByte;

  UNUSED(Index);

  /* work around the parser problem related to the ' character */

  if (!as_strncasecmp(ArgStr[2].str.p_str, "AF\'", 3))
    ArgStr[2].str.p_str[3] = '\0';

  if (!ChkArgCnt(2, 2));
  else if (ParPair("AF", "AF\'"))
  {
    BAsmCode[0] = 0x08;
    CodeLen = 1;
  }
  else if (ParPair("AF", "AF`"))
  {
    BAsmCode[0] = 0x08;
    CodeLen = 1;
  }
  else
  {
    if ((ArgStr[2].str.p_str[0]) && (ArgStr[2].str.p_str[strlen(ArgStr[2].str.p_str) - 1] == '\''))
    {
      OK = True;
      ArgStr[2].str.p_str[strlen(ArgStr[2].str.p_str) - 1] = '\0';
    }
    else
      OK = False;

    DecodeAdr(&ArgStr[1], MModReg8 | MModReg16 | MModSPRel | MModIndReg16);
    switch (AdrMode)
    {
      case ModReg8:
        if (AdrPart == 6)
        {
          if (PrefixCnt) WrStrErrorPos(ErrNum_InvAddrMode, &ArgStr[1]);
          else if (DecodeAdr_A(&ArgStr[2])) /* (HL),A */
          {
            BAsmCode[0] = 0xed;
            BAsmCode[1] = 0x37;
            CodeLen = 2;
          }
        }
        else
        {
          AdrByte = AdrPart;
          DecodeAdr(&ArgStr[2], MModReg8);
          switch (AdrMode)
          {
            case ModReg8:
              if (AdrPart == 6)
              {
                if ((AdrByte != AccReg) || PrefixCnt) WrStrErrorPos(ErrNum_InvAddrMode, &ArgStr[2]); /* A<->(HL) */
                else
                {
                  BAsmCode[0] = 0xed;
                  BAsmCode[1] = 0x37;
                  CodeLen = 2;
                }
              }
              else if ((AdrByte == AccReg) && !OK)
              {
                BAsmCode[0] = 0xed;
                BAsmCode[1] = 0x07 + (AdrPart << 3);
                CodeLen = 2;
              }
              else if ((AdrPart == AccReg) && !OK)
              {
                BAsmCode[0] = 0xed;
                BAsmCode[1] = 0x07 + (AdrByte << 3);
                CodeLen = 2;
              }
              else if (OK && (AdrPart == AdrByte))
              {
                BAsmCode[0] = 0xcb;
                BAsmCode[1] = 0x30 + AdrPart;
                CodeLen = 2;
              }
              else WrStrErrorPos(ErrNum_InvAddrMode, &ArgStr[2]);
              break;
            default:
              break;
          }
        }
        break;
      case ModReg16:
        if (AdrPart == 3) WrStrErrorPos(ErrNum_InvAddrMode, &ArgStr[1]);
        else if (PrefixCnt == 0) /* EX R16,... */
        {
          AdrByte = (AdrPart == HLReg) ? SPReg : AdrPart;
          DecodeAdr(&ArgStr[2], MModReg16 | ((AdrPart == HLReg) ? MModSPRel : 0));
          switch (AdrMode)
          {
            case ModReg16:
              if (AdrPart == 3) WrStrErrorPos(ErrNum_InvAddrMode, &ArgStr[2]);
              else if (OK)
              {
                if (AdrPart == 2)
                  AdrPart = 3;
                if ((PrefixCnt != 0) || (AdrPart != AdrByte)) WrStrErrorPos(ErrNum_InvAddrMode, &ArgStr[2]);
                else
                {
                  BAsmCode[0] = 0xed;
                  BAsmCode[1] = 0xcb;
                  BAsmCode[2] = 0x30 + AdrByte;
                  CodeLen = 3;
                }
              }
              else if (PrefixCnt == 0)
              {
                if (AdrByte == 0)
                {
                  if (AdrPart == 2)
                    AdrPart = 3;
                  BAsmCode[0] = 0xed;
                  BAsmCode[1] = 0x01 + (AdrPart << 2);
                  CodeLen = 2;
                }
                else if (AdrPart == 0)
                {
                  BAsmCode[0] = 0xed;
                  BAsmCode[1] = 0x01 + (AdrByte << 2);
                  CodeLen = 2;
                }
              }
              else
              {
                if (AdrPart == 2)
                  AdrPart = 3;
                BAsmCode[1] = 0x03 + ((BAsmCode[0] >> 2) & 8) + (AdrByte << 4);
                BAsmCode[0] = 0xed;
                CodeLen = 2;
              }
              break;
            case ModSPRel:
              if ((AdrCnt == 1) && !AdrVals[0]) /* HL <-> (SP) */
              {
                BAsmCode[PrefixCnt] = 0xe3;
                CodeLen = PrefixCnt + 1;
              }
              else
                WrStrErrorPos(ErrNum_InvAddrMode, &ArgStr[2]);
              break;
            default:
              break;
          }
        }
        else /* EX XY,... */
        {
          DecodeAdr(&ArgStr[2], MModReg16 | MModSPRel);
          switch (AdrMode)
          {
            case ModReg16:
              if (AdrPart == 3) WrStrErrorPos(ErrNum_InvAddrMode, &ArgStr[2]);
              else if (OK)
              {
                if ((PrefixCnt != 2) || (BAsmCode[0] != BAsmCode[1])) WrStrErrorPos(ErrNum_InvAddrMode, &ArgStr[2]);
                else
                {
                  BAsmCode[2] = ((BAsmCode[0] >> 5) & 1)+0x34;
                  BAsmCode[0] = 0xed;
                  BAsmCode[1] = 0xcb;
                  CodeLen = 3;
                }
              }
              else if (PrefixCnt == 1)
              {
                if (AdrPart == 2)
                  AdrPart = 3;
                BAsmCode[1] = ((BAsmCode[0] >> 2) & 8) + 3 + (AdrPart << 4);
                BAsmCode[0] = 0xed;
                CodeLen = 2;
              }
              else if (BAsmCode[0] == BAsmCode[1]) WrStrErrorPos(ErrNum_InvAddrMode, &ArgStr[2]);
              else
              {
                BAsmCode[0] = 0xed;
                BAsmCode[1] = 0x2b;
                CodeLen = 2;
              }
              break;
            case ModSPRel:
              if ((AdrCnt == 1) && !AdrVals[0]) /* IX/IX <-> (SP) */
              {
                BAsmCode[PrefixCnt] = 0xe3;
                CodeLen = PrefixCnt + 1;
              }
              else
                WrStrErrorPos(ErrNum_InvAddrMode, &ArgStr[2]);
              break;
            default:
              break;
          }
        }
        break;
      case ModSPRel:
        if ((AdrCnt != 1) || AdrVals[0]) WrStrErrorPos(ErrNum_InvAddrMode, &ArgStr[1]);
        else switch (DecodeAdr(&ArgStr[2], MModReg16))
        {
          case ModReg16: /* (SP) <-> HL/IX/IX */
            if (AdrPart != HLReg) WrStrErrorPos(ErrNum_InvAddrMode, &ArgStr[2]);
            else
            {
              BAsmCode[PrefixCnt] = 0xe3;
              CodeLen = PrefixCnt + 1;
            }
            break;
          default:
            break;
        }
        break;
      default:
        break;
    }
  }
}

static void DecodeIN_OUT(Word IsOUT)
{
  if ((ArgCnt == 1) && !IsOUT)
  {
    if (DecodeAdr(&ArgStr[1], MModIndReg8) == ModIndReg8)
    {
      BAsmCode[0] = 0xed;
      BAsmCode[1] = 0x70;
      CodeLen = 2;
    }
  }
  else if (ChkArgCnt(2, 2))
  {
    const tStrComp *pPortArg = IsOUT ? &ArgStr[1] : &ArgStr[2],
                   *pRegArg = IsOUT ? &ArgStr[2] : &ArgStr[1];

    /* allow absolute I/O address also without (...) */

    OpSize = 0;
    switch (DecodeAdr(pPortArg, MModIndReg8 | MModIOAbs | MModImm))
    {
      case ModIndReg16:
        if (0 != AdrPart) WrStrErrorPos(ErrNum_InvAddrMode, pPortArg);
        else if (ModReg8 == DecodeAdr(pRegArg, MModReg8))
        {
          CodeLen = 2;
          BAsmCode[0] = 0xed;
          BAsmCode[1] = 0x40 | (AdrPart << 3) | !!IsOUT;
        }
        break;
      case ModIndReg8:
        DecodeAdrWithF(pRegArg, !IsOUT);
        switch (AdrMode)
        {
          case ModReg8:
            if (PrefixCnt != 0) WrError(ErrNum_InvAddrMode);
            else
            {
              CodeLen = 2;
              BAsmCode[0] = 0xed;
              BAsmCode[1] = 0x40 + (AdrPart << 3);
              if (IsOUT)
                BAsmCode[1]++;
            }
            break;
          case ModImm:
            if (!IsOUT) WrError(ErrNum_InvAddrMode);
            else if (AdrVals[0] == 0)
            {
              BAsmCode[0] = 0xed;
              BAsmCode[1] = 0x71;
              CodeLen = 2;
            }
            else
            {
              BAsmCode[0] = 0xed;
              BAsmCode[1] = 0x71;
              BAsmCode[2] = AdrVals[0];
              CodeLen = 3;
            }
            break;
          default:
            if (AdrMode != ModNone) WrError(ErrNum_InvAddrMode);
        }
        break;
      case ModIOAbs:
      case ModImm:
        if (DecodeAdr_A(pRegArg))
        {
          CodeLen = 2;
          BAsmCode[0] = IsOUT ? 0xd3 : 0xdb;
          BAsmCode[1] = AdrVals[0];
        }
        break;
      default:
        break;
    }
  }
}

static void DecodeINW_OUTW(Word IsOUTW)
{
  const tStrComp *pPortArg, *pRegArg;

  if (!ChkArgCnt(2, 2))
    return;

  pPortArg = IsOUTW ? &ArgStr[1] : &ArgStr[2];
  pRegArg  = IsOUTW ? &ArgStr[2] : &ArgStr[1];

  if (DecodeAdr(pPortArg, MModIndReg8) != ModIndReg8)
    return;

  OpSize = 1;
  switch (DecodeAdr(pRegArg, MModReg16 | (IsOUTW ? MModImm : 0)))
  {
    case ModReg16:
      if ((AdrPart == 3) || (PrefixCnt > 0)) WrError(ErrNum_InvAddrMode);
      else
      {
        switch (AdrPart)
        {
          case 1: AdrPart = 2; break;
          case 2: AdrPart = 7; break;
        }
        BAsmCode[0] = 0xdd;
        BAsmCode[1] = 0x40 + (AdrPart << 3);
        if (IsOUTW)
          BAsmCode[1]++;
        CodeLen = 2;
      }
      break;
    case ModImm:
      CodeLen = 0;
      BAsmCode[CodeLen++] = 0xfd;
      BAsmCode[CodeLen++] = 0x79;
      AppendAdrVals();
      break;
    default:
      break;
  }
}

/*!------------------------------------------------------------------------
 * \fn     DecodeIN0_OUT0(Word IsOUT0)
 * \brief  Handle IN0/OUT0 instructions
 * \param  IsOUT0 1 for OUT0, 0 for IN0
 * ------------------------------------------------------------------------ */

static void DecodeIN0_OUT0(Word IsOUT0)
{
  /* 'IN0 (C)' better should not be allowed at all, because it was a copy'n'waste from
     the undocumented Z80 'IN (C)' which should better have been named 'IN F,(C)'.  But
     I will leave it in for upward compatibility, and not implicitly assume A as register: */

  if (ChkArgCnt(IsOUT0 ? 2 : 1, 2))
  {
    Boolean OK;
    const tStrComp *pRegArg, *pPortArg;

    if (IsOUT0)
    {
      pRegArg = (ArgCnt == 2) ? &ArgStr[2] : NULL;
      pPortArg = &ArgStr[1];
    }
    else
    {
      pRegArg = (ArgCnt == 2) ? &ArgStr[1] : NULL;
      pPortArg = &ArgStr[ArgCnt];
    }
    OpSize = 0;
    if (!pRegArg)
    {
      AdrPart = 6;
      OK = True;
    }
    else
    {
      DecodeAdrWithF(pRegArg, !IsOUT0);
      if ((AdrMode == ModReg8) && (PrefixCnt == 0)) OK = True;
      else
      {
        OK = False;
        if (AdrMode != ModNone) WrStrErrorPos(ErrNum_InvAddrMode, pRegArg);
      }
    }
    if (OK)
    {
      BAsmCode[2] = EvalStrIntExpression(pPortArg, UInt8, &OK);
      if (OK)
      {
        BAsmCode[0] = 0xed;
        BAsmCode[1] = AdrPart << 3;
        if (IsOUT0)
          BAsmCode[1]++;
        CodeLen = 3;
      }
    }
  }
}

static void DecodeINA_INAW_OUTA_OUTAW(Word Code)
{
  Word IsIn = Code & 8;
  LongWord AdrLong;
  tStrComp *pRegArg, *pPortArg;
  tEvalResult EvalResult;

  if (!ChkArgCnt(2, 2))
    return;

  pRegArg = IsIn ? &ArgStr[1] : &ArgStr[2];
  pPortArg = IsIn ? &ArgStr[2] : &ArgStr[1];

  OpSize = Code & 1;
  if (!(OpSize ? DecodeAdr_HL(pRegArg) : DecodeAdr_A(pRegArg)))
    return;

  AdrLong = EvalStrIntExpressionWithResult(pPortArg, ExtFlag ? Int32 : UInt8, &EvalResult);
  if (EvalResult.OK)
  {
    ChkSpace(SegIO, EvalResult.AddrSpaceMask);
    if (AdrLong > 0xfffffful)
      ChangeDDPrefix(ePrefixIW);
    else if (AdrLong > 0xfffful)
      ChangeDDPrefix(ePrefixIB);
    CodeLen = PrefixCnt;
    BAsmCode[CodeLen++] = 0xed + (OpSize << 4);
    BAsmCode[CodeLen++] = 0xd3 + IsIn;
    BAsmCode[CodeLen++] = AdrLong & 0xff;
    BAsmCode[CodeLen++] = (AdrLong >> 8) & 0xff;
    if (AdrLong > 0xfffful)
      BAsmCode[CodeLen++] = (AdrLong >> 16) & 0xff;
    if (AdrLong > 0xfffffful)
      BAsmCode[CodeLen++] = (AdrLong >> 24) & 0xff;
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

static void encode_jp_core(Byte condition)
{
  BAsmCode[PrefixCnt] = 0xc2 + condition;
  CodeLen = PrefixCnt + 1;
  AppendAdrVals();
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
  Boolean check_jr = False;

  UNUSED(Code);

  switch (ArgCnt)
  {
    case 1:
      Cond = 1;
      check_jr = True;
      break;
    case 2:
      if (!DecodeCondition(ArgStr[1].str.p_str, &Cond))
      {
        WrStrErrorPos(ErrNum_UndefCond, &ArgStr[1]);
        return;
      }
      check_jr = (Cond <= 3);
      Cond <<= 3;
      break;
    default:
      (void)ChkArgCnt(1, 2);
      return;
  }

  switch (DecodeAdr(&ArgStr[ArgCnt], MModImmIsAbs | MModAbs | ((Cond == 1) ? MModReg8 : 0)))
  {
    case ModReg8:
      if ((AdrPart != 6) || ((AdrCnt > 0) && AdrVals[0])) WrStrErrorPos(ErrNum_InvAddrMode, &ArgStr[ArgCnt]);
      else
      {
        BAsmCode[PrefixCnt] = 0xe9;
        CodeLen = PrefixCnt + 1;
      }
      break;
    case ModAbs:
    {
      LongInt dist;

      if ((AttrPartOpSize[0] == eSymbolSize24Bit) && (AttrPartOpSize[1] == eSymbolSize16Bit))
      {
        WrStrErrorPos(ErrNum_UndefAttr, &AttrPart);
        return;
      }

      if (check_jr
       && (get_jr_dist(AdrVal, &dist) != UInt0)
       && !mFirstPassUnknownOrQuestionable(adr_val_flags))
        WrStrErrorPos(ErrNum_RelJumpPossible, &ArgStr[ArgCnt]);
      encode_jp_core(Cond);
      break;
    }
  }
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

  /* M80 compatibility: DEFB->DB, DEFW->DW */

  strmov(OpPart.str.p_str + 1, OpPart.str.p_str + 3);
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

static void AddAcc(const char *NewName, cpu_core_mask_t core_mask, Word NewCode)
{
  order_array_rsv_end(AccOrders, BaseOrder);
  AccOrders[InstrZ].core_mask = core_mask;
  AccOrders[InstrZ].Code = NewCode;
  AddInstTable(InstTable, NewName, InstrZ++, DecodeAcc);
}

static void AddHL(const char *NewName, cpu_core_mask_t core_mask, Word NewCode)
{
  order_array_rsv_end(HLOrders, BaseOrder);
  HLOrders[InstrZ].core_mask = core_mask;
  HLOrders[InstrZ].Code = NewCode;
  AddInstTable(InstTable, NewName, InstrZ++, DecodeHL);
}

static void AddALU(const char *Name8, const char *Name16, Byte Code)
{
  AddInstTable(InstTable, Name8 , Code, DecodeALU8);
  AddInstTable(InstTable, Name16, Code, DecodeALU16);
}

static void AddShift(const char *Name8, const char *Name16, Byte Code)
{
  AddInstTable(InstTable, Name8 , Code, DecodeShift8);
  if (Name16)
    AddInstTable(InstTable, Name16, Code, DecodeShift16);
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

  AddInstTable(InstTable, "LD" , 0, DecodeLD);
  AddInstTable(InstTable, "LDW", 1, DecodeLD);
  AddInstTable(InstTable, "LDX", 0, DecodeLDX);
  AddInstTable(InstTable, "ADD", 0, DecodeADD);
  AddInstTable(InstTable, "INC" , 0, DecodeINC_DEC);
  AddInstTable(InstTable, "DEC" , 1, DecodeINC_DEC);
  AddInstTable(InstTable, "MLT" , 0, DecodeMLT);
  AddInstTable(InstTable, "DIVUW" , 0x28, DecodeMULT_DIV);
  AddInstTable(InstTable, "MULTW" , 0x00, DecodeMULT_DIV);
  AddInstTable(InstTable, "MULTUW", 0x08, DecodeMULT_DIV);
  AddInstTable(InstTable, "TST", 0, DecodeTST);
  AddInstTable(InstTable, "PUSH", 4, DecodePUSH_POP);
  AddInstTable(InstTable, "POP" , 0, DecodePUSH_POP);
  AddInstTable(InstTable, "EX"  , 0, DecodeEX);
  AddInstTable(InstTable, "IN"  , 0, DecodeIN_OUT);
  AddInstTable(InstTable, "OUT" , 1, DecodeIN_OUT);
  AddInstTable(InstTable, "INW"  , 0, DecodeINW_OUTW);
  AddInstTable(InstTable, "OUTW" , 1, DecodeINW_OUTW);
  AddInstTable(InstTable, "IN0"  , 0, DecodeIN0_OUT0);
  AddInstTable(InstTable, "OUT0" , 1, DecodeIN0_OUT0);
  AddInstTable(InstTable, "INA"  , 8, DecodeINA_INAW_OUTA_OUTAW);
  AddInstTable(InstTable, "INAW" , 9, DecodeINA_INAW_OUTA_OUTAW);
  AddInstTable(InstTable, "OUTA" , 0, DecodeINA_INAW_OUTA_OUTAW);
  AddInstTable(InstTable, "OUTAW", 1, DecodeINA_INAW_OUTA_OUTAW);
  AddInstTable(InstTable, "RET" , 0, DecodeRET);
  AddInstTable(InstTable, "JP" , 0, DecodeJP);
  AddInstTable(InstTable, "CALL", 0, DecodeCALL);
  AddInstTable(InstTable, "JR" , 0, DecodeJR);
  AddInstTable(InstTable, "DJNZ", 0, DecodeDJNZ);
  AddInstTable(InstTable, "RST", 0, DecodeRST);
  AddInstTable(InstTable, "DI", 0, DecodeEI_DI);
  AddInstTable(InstTable, "EI", 8, DecodeEI_DI);
  AddInstTable(InstTable, "IM", 0, DecodeIM);
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

  InstrZ = 0;
  AddAcc("CPL"  , e_core_mask_all, 0x002f);
  AddAcc("NEG"  , e_core_mask_all, 0xed44);
  AddAcc("EXTS" , e_core_mask_all, 0xed65);

  InstrZ = 0;
  AddHL("CPLW" , e_core_mask_all, 0xdd2f);
  AddHL("NEGW" , e_core_mask_all, 0xed54);
  AddHL("EXTSW", e_core_mask_all, 0xed75);

  AddALU("SUB", "SUBW", 2);
  AddALU("AND", "ANDW", 4);
  AddALU("OR" , "ORW" , 6);
  AddALU("XOR", "XORW", 5);
  AddALU("CP" , "CPW" , 7);

  AddShift("RLC",  "RLCW", 0);
  AddShift("RRC",  "RRCW", 1);
  AddShift("RL",   "RLW",  2);
  AddShift("RR",   "RRW",  3);
  AddShift("SLA",  "SLAW", 4);
  AddShift("SRA",  "SRAW", 5);
  AddShift("SRL",  "SRLW", 7);

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
 * \fn     DissectReg_Z80(char *p_dest, size_t dest_size, tRegInt value, tSymbolSize inp_size)
 * \brief  dissect register symbols - Z80 variant
 * \param  p_dest destination buffer
 * \param  dest_size destination buffer size
 * \param  value numeric register value
 * \param  inp_size register size
 * ------------------------------------------------------------------------ */

static void DissectReg_Z80(char *p_dest, size_t dest_size, tRegInt value, tSymbolSize inp_size)
{
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

static void InternSymbol_Z80(char *p_arg, TempResult *p_result)
{
  Byte reg_num;

  if (DecodeReg8Core(p_arg, &reg_num))
  {
    p_result->Typ = TempReg;
    p_result->DataSize = eSymbolSize8Bit;
    p_result->Contents.RegDescr.Reg = reg_num;
    p_result->Contents.RegDescr.Dissect = DissectReg_Z80;
    p_result->Contents.RegDescr.compare = NULL;
  }
  else if (DecodeReg16Core(p_arg, &reg_num))
  {
    p_result->Typ = TempReg;
    p_result->DataSize = eSymbolSize16Bit;
    p_result->Contents.RegDescr.Reg = reg_num;
    p_result->Contents.RegDescr.Dissect = DissectReg_Z80;
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
  InternSymbol = InternSymbol_Z80;
  SwitchFrom = DeinitFields; InitFields();
  DissectReg = DissectReg_Z80;

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
