/*
 * Copyright (c) 1997-2018, NVIDIA CORPORATION.  All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

/**
 * \file
 * \brief SCFTN semantic analyzer module element.
 */

#include "gbldefs.h"
#include "error.h"
#include "global.h"
#include "semant.h"
#include "symtab.h"
#include "ilm.h"
#include "ilmtp.h"
#include "machar.h"
#include "outliner.h"

extern void scan_include();
extern void scan_options();
extern void chkstruct();
extern void assign();
extern void addlabel();

#define SERROR(e, f, c)                 \
  {                                     \
    char bf[20];                        \
    sprintf(bf, f, c);                  \
    error(e, 2, gbl.lineno, bf, CNULL); \
  }

#define ERR170(s) error(170, 2, gbl.lineno, s, CNULL)

/**
   \brief Initialize semantic analyzer for new user subprogram unit.
 */
void
semant_reinit()
{
  if (flg.smp && llvm_ilms_rewrite_mode()) {
  } else
      if (gbl.ilmfil)
    (void)fseek(gbl.ilmfil, 0L, 0);

  ilmb.ilmavl = BOS_SIZE;
  NEW(ilmb.ilm_base, ILM_T, ilmb.ilm_size);
  sem.wrilms = (flg.code | flg.object);
  sem.eqvlist = EQVV_END;
  sem.pgphase = 0;
  sem.flabels = 0; /* not NOSYM - a sym's SYMLK is init'd to NOSYM. if
                    * its SYMLK is NOSYM, then it hasn't been added */
  sem.nml = NOSYM;
  sem.atemps = 0;
  sem.ptemps = 0;
  sem.ctemps = 0;
  sem.savall = flg.save;
  sem.savloc = FALSE;
  sem.psfunc = FALSE;
  sem.in_stfunc = FALSE;
  sem.dinit_error = FALSE;
  sem.dinit_data = FALSE;
  sem.none_implicit = flg.dclchk;
  sem.vf_expr.temps = 0;
  sem.vf_expr.labels = 0;
  sem.ignore_stmt = FALSE;
  sem.switch_avl = 0;
  sem.temps_reset = FALSE;
  sem.p_adjarr = NOSYM;
  sem.gdtype = -1;
  sem.atomic.seen = sem.atomic.pending = FALSE;
  sem.parallel = FALSE;
  sem.expect_do = FALSE;
  sem.close_pdo = FALSE;
  sem.sc = SC_LOCAL;
  sem.scope = 0;
}

/** \brief Initialize semantic analyzer.
 */
void
semant_init(void)
{
  /* set ilmb.ilm_size, then call re_init */
  ilmb.ilm_size = 1000;
  semant_reinit();
}

/**
   \brief Dereference an ilm pointer to determine the rvalue i.e. its
   symbol pointer.
 */
int getrval(ilmptr) int ilmptr;
{
  int opr1 = ILMA(ilmptr + 1);
  int opr2 = ILMA(ilmptr + 2);

  switch (ILMA(ilmptr)) {
  case IM_NSUBS:
  case IM_SUBS:
    return (getrval(opr1));

  case IM_ELEMENT:
    return (getrval(opr2));
    break;

  case IM_BASE:
    return opr1;

  case IM_MEMBER:
    return opr2;

  case IM_IFUNC:
  case IM_KFUNC:
  case IM_RFUNC:
  case IM_DFUNC:
  case IM_CFUNC:
  case IM_CDFUNC:
  case IM_CALL:
  case IM_IVFUNCA:
  case IM_KVFUNCA:
  case IM_RVFUNCA:
  case IM_DVFUNCA:
  case IM_CVFUNCA:
  case IM_CDVFUNCA:
  case IM_VCALLA:
    return opr2;

  case IM_IFUNCA:
  case IM_KFUNCA:
  case IM_RFUNCA:
  case IM_DFUNCA:
  case IM_CFUNCA:
  case IM_CDFUNCA:
  case IM_CALLA:
  case IM_PIFUNCA:
  case IM_PKFUNCA:
  case IM_PRFUNCA:
  case IM_PDFUNCA:
  case IM_PCFUNCA:
  case IM_PCDFUNCA:
  case IM_PCALLA:
    return getrval(opr2);

  case IM_PLD:
    return opr2;

  default:
    return 0;
  }
}

/**
   \brief Convert a hollerith constant to a numeric value.  The
   destination can be any one of the 5 available sizes 1 byte, 2
   bytes, 4 bytes, 8 bytes, or 16 bytes.

   \param cp is a character pointer to hollerith character string.
   \param num is the result of conversion of hollerith to numeric.
   \param bc is the byte count of destination area i.e. *1, *2, *4, *8 or *16
 */
static void
holtonum(char *cp, INT num[4], int bc)
{
  unsigned char *p, buf[18];
  int sc, i;
  int lc;

  /*
   * There are 4 32-bit parcels.  Index 'i' starts at the parcel to begin
   * filling and moves upward.  For example, for a 8 byte quantity 'i' would
   * start at 2 and end at 3 thus the last two words of 'num' array contain
   * the 64-bit number.
   */
  num[0] = num[1] = num[2] = num[3] = 0;
  sprintf((char *)buf, "%-17.17s", cp); /* Need 1 xtra char to detect trunc */
  p = buf;
  /* Select the initial parcel based on size of destination area */
  i = 3;
  if (bc > 4)
    i = 2;
  if (bc > 8)
    i = 0;
  if (flg.endian) {
    /*
     * The big endian byte order simply shifts each new character left 8
     * bits FEWER than the previous shifted character producing the order
     * ABCDEF...
     */
    while (i <= 3) {
      sc = (bc < 4) ? bc : 4; /* Initial shift count */
      while (sc--)
        num[i] |= *p++ << (sc * 8);
      i++;
    }
  } else {
    /*
     * The little endian byte order simply shifts each new character left 8
     * bits MORE than the previous shifted character producing the order
     * ...FEDCBA
     */
    while (i <= 3) {
      sc = (bc < 4) ? bc : 4; /* Initial shift count */
      lc = sc - 1;
      while (sc--)
        num[i] |= *p++ << ((lc - sc) * 8);
      i++;
    }
  }

  if (*p != '\0' && *p != ' ')
    errwarn(24);
}

/**
   \brief Convert doubleword hex/octal value to a character.

   Function return value is the symbol table entry of the character
   constant.  The conversion is performed by copying an 8-bit value (2
   hex digits) to a character position which is endian-dependent.  The
   endian-dependency is handled as if the hex value is "equivalenced"
   with a character value of the same length.  The length of the
   character constant returned is determined by the magnitude of the
   hex values (leading 0's are not converted).  Note that this
   conversion returns the same character value in context of an
   assignment or data initialization.  We may be incompatible with
   other implementations with respect to data initialization:
   1. if the value is smaller than the char item being initialized,
      the conversion process results in appending blanks; other
      systems may pad with 'nulls'
   2. if the value is larger, truncation of the least significant
      characters  ("rightmost") occurs; other systems truncate the
      most significant characters ("leftmost").

   hexval[0] is msw, hexval[1] is lsw.
 */
static int
hex2char(INT hexval[2])
{
  UINT val;
  int i;
  int len;
  char *p;
  char buf[8];

  len = 0;
  if (flg.endian) {
    /* big endian: rightmost 2 hex digits are in last byte position */
    p = buf + 7;
    i = -1;
  } else {
    /* little endian: rightmost 2 hex digits are in first byte position */
    p = buf;
    i = 1;
  }
  val = hexval[1];
  while (val) {
    *p = val & 0xff;
    p += i;
    len++;
    val >>= 8;
  }
  val = hexval[0];
  while (val) {
    *p = val & 0xff;
    p += i;
    len++;
    val >>= 8;
  }

  if (len == 0) {
    len = 1;
    *p = '\0';
  } else if (flg.endian)
    p++;
  else
    p = buf;

  return getstring(p, len);
}

/**
   \brief Convert doubleword hex/octal value to an ncharacter.

   Function return value is the symbol table entry of the character
   constant.  The conversion is performed by copying an 8-bit value (2
   hex digits) to a character position which is endian-dependent. The
   endian-dependency is handled as if the hex value is "equivalenced"
   with a ncharacter value of the same length.  The length of the
   ncharacter constant returned is determined by the magnitude of the
   hex values (leading 0's are not converted).  Note that this
   conversion returns the same ncharacter value in context of an
   assignment or data initialization.  We may be incompatible with
   other implementations with respect to data initialization:
   1. if the value is smaller than the nchar item being initialized,
      the conversion process results in appending blanks; other
      systems may pad with 'nulls'
   2. if the value is larger, truncation of the least significant
      characters ("rightmost") occurs; other systems truncate the most
      significant characters ("leftmost").

   hexval[0] is msw, hexval[1] is lsw.
 */
static int
hex2nchar(INT hexval[2])
{
  UINT val;
  int i;
  int len;
  unsigned short *p;
  unsigned short buf[4];

  len = 0;
  if (flg.endian) {
    /* big endian: rightmost 2 hex digits are in last byte position */
    p = buf + 3;
    i = -1;
  } else {
    /* little endian: rightmost 2 hex digits are in first byte position */
    p = buf;
    i = 1;
  }
  val = hexval[1];
  while (val) {
    *p = val & 0xffff;
    p += i;
    len += 2;
    val >>= 16;
  }
  val = hexval[0];
  while (val) {
    *p = val & 0xffff;
    p += i;
    len += 2;
    val >>= 16;
  }
  if (len == 0) {
    len = 1;
    *p = '\0';
  } else if (flg.endian)
    p++;
  else
    p = buf;

  return getstring((char *)p, len);
}

/**
 * \brief Convert constant from oldtyp to newtyp.
 *
 * Issue error messages only for impossible conversions.  Return constant value
 * for 32-bit constants, or symbol table pointer.  Can only be used for scalar
 * constants.
 *
 * Remember: Non-decimal constants are octal, hexadecimal, or hollerith
 * constants which are represented by DT_WORD, DT_DWORD and DT_HOLL.
 * Non-decimal constants 'assume' data types rather than go thru a conversion.
 * Hollerith constants have a data type of DT_HOLL in the semantic stack,
 * however, the symbol table entry they point to has a data type of DT_CHAR.
 *
 * Hollerith constants are always treated as scalars while octal or
 * hexadecimal constants can be promoted to vectors.
 */
INT
cngcon(INT oldval, int oldtyp, int newtyp)
{
  int to, from;
  char *cp, buf[20];
  int newcvlen, oldcvlen, msk, blnk;
  INT num[4], result;
  INT num1[4];
  INT swap;
  if (is_empty_typedef(newtyp) && oldtyp == DT_INT) {
    /* FS#17600 - special case for emptyy derived type */
    newtyp = DT_INT;
  }

  if (newtyp == oldtyp)
    return oldval;
  to = DTY(newtyp);
  from = DTY(oldtyp);
  if (!TY_ISSCALAR(to) || !TY_ISSCALAR(from))
    goto type_conv_error;

  switch (to) {
  case TY_WORD:
  case TY_DWORD:
    return oldval;

  case TY_BLOG:
  case TY_BINT:
    /* decimal integer constants are 32-bits, BUT, PARAMETER
        may be TY_SLOG, TY_SINT, TY_BLOG, or TY_BINT.
     */
    switch (from) {
    case TY_WORD:
      if (oldval & 0xFFFFFF00)
        errwarn(15);
      return (ARSHIFT(LSHIFT(oldval, 24), 24));
    case TY_DWORD:
      result = CONVAL2G(oldval);
      if (CONVAL1G(oldval) || (result & 0xFFFFFF00))
        errwarn(15);
      return (ARSHIFT(LSHIFT(result, 24), 24));
    case TY_INT8:
    case TY_LOG8:
      result = CONVAL2G(oldval);
      if ((((result & 0xFFFFFF80) != 0xFFFFFF80) && (result & 0xFFFFFF00)) ||
          (CONVAL1G(oldval) && CONVAL1G(oldval) != 0xFFFFFFFF))
        SERROR(128, "%d", result & 0xFF);
      return (ARSHIFT(LSHIFT(result, 24), 24));
    case TY_BINT:
    case TY_SINT:
    case TY_INT:
    case TY_BLOG:
    case TY_SLOG:
    case TY_LOG:
      if (((oldval & 0xFFFFFF80) != 0xFFFFFF80) && (oldval & 0xFFFFFF00))
        SERROR(128, "%d", oldval & 0xFF);
      return (ARSHIFT(LSHIFT(oldval, 24), 24));
    default:
      goto other_int_cases;
      break;
    }
    break;
  case TY_SLOG:
  case TY_SINT:
    switch (from) {
    case TY_WORD:
      if (oldval & 0xFFFF0000)
        errwarn(15);
      return (ARSHIFT(LSHIFT(oldval, 16), 16));
    case TY_DWORD:
      result = CONVAL2G(oldval);
      if (CONVAL1G(oldval) || (result & 0xFFFF0000))
        errwarn(15);
      return (ARSHIFT(LSHIFT(result, 16), 16));
    case TY_INT8:
    case TY_LOG8:
      result = CONVAL2G(oldval);
      if ((((result & 0xFFFF8000) != 0xFFFF8000) && (result & 0xFFFF0000)) ||
          (CONVAL1G(oldval) && CONVAL1G(oldval) != 0xFFFFFFFF))
        SERROR(128, "%d", result & 0xFFFF);
      return (ARSHIFT(LSHIFT(result, 16), 16));
    case TY_BINT:
    case TY_SINT:
    case TY_INT:
    case TY_BLOG:
    case TY_SLOG:
    case TY_LOG:
      if (((oldval & 0xFFFF8000) != 0xFFFF8000) && (oldval & 0xFFFF0000))
        SERROR(128, "%d", oldval & 0xFFFF);
      return (ARSHIFT(LSHIFT(oldval, 16), 16));
    default:
      goto other_int_cases;
      break;
    }
    break;
  case TY_LOG:
  case TY_INT:
    if (from == TY_DWORD) {
      result = CONVAL2G(oldval);
      if (CONVAL1G(oldval))
        errwarn(15);
      return (result);
    }
    if (from == TY_INT8) {
      result = CONVAL2G(oldval);
      if (CONVAL1G(oldval) && (CONVAL1G(oldval) != 0xFFFFFFFF))
        SERROR(128, "%d", CONVAL1G(oldval));
      return (result);
    }
    if (from == TY_LOG8) {
      result = CONVAL2G(oldval);
      return (result);
    } else if (from == TY_WORD || TY_ISINT(from))
      return oldval;
    else {
    other_int_cases:
      switch (from) {
      case TY_CMPLX:
        oldval = CONVAL1G(oldval);
      case TY_REAL:
        xfix(oldval, &result);
        return result;
      case TY_DCMPLX:
        oldval = CONVAL1G(oldval);
      case TY_DBLE:
        num[0] = CONVAL1G(oldval);
        num[1] = CONVAL2G(oldval);
        xdfix(num, &result);
        return result;
      case TY_QUAD:
        uf("QUAD");
        return 0;
      case TY_CHAR:
        if (flg.standard)
          ERR170("conversion of CHARACTER constant to numeric");
      case TY_HOLL:
        cp = stb.n_base + CONVAL1G(oldval);
        oldcvlen = 4;
        if (to == TY_BLOG || to == TY_BINT)
          oldcvlen = 1;
        if (to == TY_SLOG || to == TY_SINT)
          oldcvlen = 2;
        if (to == TY_LOG8 || to == TY_INT8)
          oldcvlen = 8;
        holtonum(cp, num, oldcvlen);
        return num[3];
      default: /* TY_NCHAR comes here */
        break;
      }
    }
    break;

  case TY_LOG8:
  case TY_INT8:
    if (from == TY_DWORD || from == TY_INT8 || from == TY_LOG8) {
      num[0] = CONVAL1G(oldval);
      num[1] = CONVAL2G(oldval);
      return getcon(num, newtyp);
    } else if (from == TY_WORD) {
      num[0] = 0;
      num[1] = oldval;
      return getcon(num, newtyp);
    } else if (TY_ISINT(from)) {
      if (oldval < 0) {
        num[0] = -1;
        num[1] = oldval;
      } else {
        num[0] = 0;
        num[1] = oldval;
      }
      return getcon(num, newtyp);
    } else {
      switch (from) {
      case TY_CMPLX:
        oldval = CONVAL1G(oldval);
      case TY_REAL:
        xfix64(oldval, num);
        return getcon(num, newtyp);
      case TY_DCMPLX:
        oldval = CONVAL1G(oldval);
      case TY_DBLE:
        num1[0] = CONVAL1G(oldval);
        num1[1] = CONVAL2G(oldval);
        xdfix64(num1, num);
        return getcon(num, newtyp);
      case TY_QUAD:
        uf("QUAD");
        return 0;
      case TY_CHAR:
        if (flg.standard)
          ERR170("conversion of CHARACTER constant to numeric");
      case TY_HOLL:
        cp = stb.n_base + CONVAL1G(oldval);
        holtonum(cp, num, 8);
        if (flg.endian == 0) {
          /* for little endian, need to swap words in each double word
           * quantity.  Order of bytes in a word is okay, but not the
           * order of words.
           */
          swap = num[2];
          num[2] = num[3];
          num[3] = swap;
        }
        return getcon(&num[2], newtyp);
      default: /* TY_NCHAR comes here */
        break;
      }
    }
    break;

  case TY_REAL:
    if (from == TY_WORD)
      return oldval;
    else if (from == TY_DWORD) {
      result = CONVAL2G(oldval);
      if (CONVAL1G(oldval))
        errwarn(15);
      return result;
    } else if (from == TY_INT8 || from == TY_LOG8) {
      num[0] = CONVAL1G(oldval);
      num[1] = CONVAL2G(oldval);
      xflt64(num, &result);
      return result;
    } else if (TY_ISINT(from)) {
      xffloat(oldval, &result);
      return result;
    } else {
      switch (from) {
      case TY_CMPLX:
        return CONVAL1G(oldval);
      case TY_DCMPLX:
        oldval = CONVAL1G(oldval);
      case TY_DBLE:
        num[0] = CONVAL1G(oldval);
        num[1] = CONVAL2G(oldval);
        xsngl(num, &result);
        return result;
      case TY_QUAD:
        uf("QUAD");
        return oldval;
      case TY_CHAR:
        if (flg.standard)
          ERR170("conversion of CHARACTER constant to numeric");
      case TY_HOLL:
        cp = stb.n_base + CONVAL1G(oldval);
        holtonum(cp, num, 4);
        return num[3];
      default:
        break;
      }
    }
    break;

  case TY_DBLE:
    if (from == TY_WORD) {
      num[0] = 0;
      num[1] = oldval;
    } else if (from == TY_DWORD) {
      num[0] = CONVAL1G(oldval);
      num[1] = CONVAL2G(oldval);
    } else if (from == TY_INT8 || from == TY_LOG8) {
      num1[0] = CONVAL1G(oldval);
      num1[1] = CONVAL2G(oldval);
      xdflt64(num1, num);
    } else if (TY_ISINT(from))
      xdfloat(oldval, num);
    else if (from == TY_DCMPLX)
      return CONVAL1G(oldval);
    else if (from == TY_CMPLX) {
      oldval = CONVAL1G(oldval);
      xdble(oldval, num);
    } else if (from == TY_REAL)
      xdble(oldval, num);
    else if (from == TY_HOLL || from == TY_CHAR) {
      if (flg.standard && from == TY_CHAR)
        ERR170("conversion of CHARACTER constant to numeric");
      cp = stb.n_base + CONVAL1G(oldval);
      holtonum(cp, num, 8);
      if (flg.endian == 0) {
        /* for little endian, need to swap words in each double word
         * quantity.  Order of bytes in a word is okay, but not the
         * order of words.
         */
        swap = num[2];
        num[2] = num[3];
        num[3] = swap;
      }
      return getcon(&num[2], DT_DBLE);
    } else if (from == TY_QUAD) {
      uf("QUAD");
      return oldval;
    } else {
      errsev(91);
      return (stb.dbl0);
    }
    return getcon(num, DT_DBLE);

  case TY_CMPLX:
    /*  num[0] = real part
     *  num[1] = imaginary part
     */
    num[1] = 0;
    if (from == TY_WORD) {
      /* a la VMS */
      num[0] = 0;
      num[1] = oldval;
    } else if (from == TY_DWORD) {
      /* a la VMS */
      num[0] = CONVAL1G(oldval);
      num[1] = CONVAL2G(oldval);
    } else if (from == TY_INT8 || from == TY_LOG8) {
      num1[0] = CONVAL1G(oldval);
      num1[1] = CONVAL2G(oldval);
      xflt64(num1, &num[0]);
    } else if (TY_ISINT(from))
      xffloat(oldval, &num[0]);
    else if (from == TY_REAL)
      num[0] = oldval;
    else if (from == TY_DBLE) {
      num1[0] = CONVAL1G(oldval);
      num1[1] = CONVAL2G(oldval);
      xsngl(num1, &num[0]);
    } else if (from == TY_DCMPLX) {
      num1[0] = CONVAL1G(CONVAL1G(oldval));
      num1[1] = CONVAL2G(CONVAL1G(oldval));
      xsngl(num1, &num[0]);
      num1[0] = CONVAL1G(CONVAL2G(oldval));
      num1[1] = CONVAL2G(CONVAL2G(oldval));
      xsngl(num1, &num[1]);
    } else if (from == TY_HOLL || from == TY_CHAR) {
      if (flg.standard && from == TY_CHAR)
        ERR170("conversion of CHARACTER constant to numeric");
      cp = stb.n_base + CONVAL1G(oldval);
      oldcvlen = DTY(DTYPEG(oldval) + 1);
      holtonum(cp, num, 8);
      return getcon(&num[2], DT_CMPLX);
    } else {
      num[0] = 0;
      num[1] = 0;
      errsev(91);
    }
    return getcon(num, DT_CMPLX);

  case TY_DCMPLX:
    if (from == TY_WORD) {
      num[0] = 0;
      num[1] = oldval;
      num[0] = getcon(num, DT_DBLE);
      num[1] = stb.dbl0;
    } else if (from == TY_DWORD) {
      num[0] = CONVAL1G(oldval);
      num[1] = CONVAL2G(oldval);
      num[0] = getcon(num, DT_DBLE);
      num[1] = stb.dbl0; /* when is stb.dbl0 set? -nzm */
    } else if (from == TY_INT8 || from == TY_LOG8) {
      num1[0] = CONVAL1G(oldval);
      num1[1] = CONVAL2G(oldval);
      xdflt64(num1, num);
      num[0] = getcon(num, DT_DBLE);
      num[1] = stb.dbl0;
    } else if (TY_ISINT(from)) {
      xdfloat(oldval, num);
      num[0] = getcon(num, DT_DBLE);
      num[1] = stb.dbl0;
    } else if (from == TY_REAL) {
      xdble(oldval, num);
      num[0] = getcon(num, DT_DBLE);
      num[1] = stb.dbl0;
    } else if (from == TY_DBLE) {
      num[0] = oldval;
      num[1] = stb.dbl0;
    } else if (from == TY_CMPLX) {
      xdble(CONVAL1G(oldval), num1);
      num[0] = getcon(num1, DT_DBLE);
      xdble(CONVAL2G(oldval), num1);
      num[1] = getcon(num1, DT_DBLE);
    } else if (from == TY_HOLL || from == TY_CHAR) {
      if (flg.standard && from == TY_CHAR)
        ERR170("conversion of CHARACTER constant to numeric");
      cp = stb.n_base + CONVAL1G(oldval);
      holtonum(cp, num1, 16);
      if (flg.endian == 0) {
        /* for little endian, need to swap words in each double word
         * quantity.  Order of bytes in a word is okay, but not the
         * order of words.
         */
        swap = num1[0];
        num1[0] = num1[1];
        num1[1] = swap;
        swap = num1[2];
        num1[2] = num1[3];
        num1[3] = swap;
      }
      num[0] = getcon(&num1[0], DT_DBLE);
      num[1] = getcon(&num1[2], DT_DBLE);
    } else if (from == TY_QUAD) {
      uf("QUAD");
      return oldval;
    } else {
      num[0] = 0;
      num[1] = 0;
      errsev(91);
    }
    return getcon(num, DT_DCMPLX);

  case TY_NCHAR:
    if (from == TY_WORD) {
      num[0] = 0;
      num[1] = oldval;
      oldval = hex2nchar(num);
      cp = stb.n_base + CONVAL1G(oldval);
      oldcvlen = kanji_len((unsigned char *)cp, DTY(DTYPEG(oldval) + 1));
      oldtyp = get_type(2, TY_NCHAR, oldcvlen);
      if (newtyp == oldtyp)
        return oldval;
    } else if (from == TY_DWORD) {
      num[0] = CONVAL1G(oldval);
      num[1] = CONVAL2G(oldval);
      oldval = hex2nchar(num);
      cp = stb.n_base + CONVAL1G(oldval);
      oldcvlen = kanji_len((unsigned char *)cp, DTY(DTYPEG(oldval) + 1));
      oldtyp = get_type(2, TY_NCHAR, oldcvlen);
      if (newtyp == oldtyp)
        return oldval;
    } else if (from != TY_NCHAR) {
      errsev(146);
      return getstring(" ", 1);
    }
    goto char_shared;

  case TY_CHAR:
    if (from == TY_WORD) {
      num[0] = 0;
      num[1] = oldval;
      oldval = hex2char(num);
      /* old value is now in character form; must changed oldtyp
       * and must check if lengths just happen to be equal.
       */
      oldtyp = DTYPEG(oldval);
      if (newtyp == oldtyp)
        return oldval;
    } else if (from == TY_DWORD) {
      num[0] = CONVAL1G(oldval);
      num[1] = CONVAL2G(oldval);
      oldval = hex2char(num);
      /* old value is now in character form; must changed oldtyp
       * and must check if lengths just happen to be equal.
       */
      oldtyp = DTYPEG(oldval);
      if (newtyp == oldtyp)
        return oldval;
    } else if (from != TY_CHAR && from != TY_HOLL) {
      errsev(146);
      return getstring(" ", 1);
    }

  char_shared:
    newcvlen = DTY(newtyp + 1);
    if (newcvlen == 0)
      return oldval;
    oldcvlen = DTY(oldtyp + 1);
    /*oldcvlen = DTY(DTYPEG(oldval)+1);	 wrong for Kanji */

    if (oldcvlen > newcvlen) {
      /* truncate character string: */
      errinfo(122);
      cp = local_sname(stb.n_base + CONVAL1G(oldval));
      if (from == TY_NCHAR ||
          (to == TY_NCHAR && (from == TY_WORD || from == TY_DWORD)))
        /* compute actual num bytes used to represent newcvlen chars:*/
        newcvlen = kanji_prefix((unsigned char *)cp, newcvlen,
                                DTY(DTYPEG(oldval) + 1));
      return getstring(cp, newcvlen);
    }

    /* oldcvlen < newcvlen -    pad with blanks.  This works for regular
       and kanji strings.  Note (from == oldcvlen) unless type is TY_NCHAR
       and there are one or more Kanji(2 byte) characters in the string. */

    newcvlen -= oldcvlen; /* number of pad blanks */
    blnk = ' ';
    if (from == TY_NCHAR) /* double for NCHAR */
      newcvlen *= 2, blnk = 0xA1;
    if (oldcvlen != 0)
      from = DTY(DTYPEG(oldval) + 1); /* number bytes in char string const */
    else
      from = 0;
    cp = getitem(0, from + newcvlen);
    BCOPY(cp, stb.n_base + CONVAL1G(oldval), char, (INT)from);
    do {
      cp[from++] = blnk;
    } while (--newcvlen > 0);
    return getstring(cp, from);

  case TY_PTR:
    if (from == TY_INT8 || from == TY_LOG8) {
      ISZ_T v;
      num[0] = CONVAL1G(oldval);
      num[1] = CONVAL2G(oldval);
      INT64_2_ISZ(num, v);
      return get_acon(0, v);
    }
    if (TY_ISINT(from)) {
      return get_acon(0, oldval);
    }
    break;

  default:
    break;
  }

type_conv_error:
  errsev(91);
  return 0;
}

/**
   \return TRUE if fortran character constant is equal to pattern (pattern is
   always uppercase)
 */
LOGICAL
sem_eq_str(int con, char *pattern)
{
  char *p1, *p2;
  int len;
  int c1, c2;

  p1 = stb.n_base + CONVAL1G(con);
  p2 = pattern;
  for (len = size_of(DTYPEG(con)); len > 0; len--) {
    c1 = *p1;
    if (c1 >= 'a' && c1 <= 'z') /* convert to upper case */
      c1 = c1 + ('A' - 'a');
    c2 = *p2;
    if (c2 == '\0' || c1 != c2)
      break;
    p1++;
    p2++;
  }

  if (len == 0)
    return TRUE;

  /*  verify that remaining characters of con are blank:  */
  while (len--)
    if (*p1++ != ' ')
      return FALSE;
  return TRUE;
}
