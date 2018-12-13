/*
 * Copyright (c) 2017-2018, NVIDIA CORPORATION.  All rights reserved.
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

/** \file
 * \brief tgtutil.c - Various definitions for the libomptarget runtime
 *
 */
#ifdef OMP_OFFLOAD_LLVM

#define _GNU_SOURCE // for vasprintf()
#include <stdio.h>
#undef _GNU_SOURCE
#include "dinit.h"
#include "kmpcutil.h"
#include "error.h"
#include "semant.h"
#include "ilmtp.h"
#include "ilm.h"
#include "ili.h"
#include "expand.h"
#include "exputil.h"
#include "outliner.h"
#include "machreg.h"
#include "mp.h"
#include "ll_structure.h"
#include "llmputil.h"
#include "iliutil.h"
#include "llutil.h"
#include "ompaccel.h"
#include "tgtutil.h"
#include "assem.h"
#include "dinitutl.h"
#include "cgllvm.h"
#include "cgmain.h"
#include "regutil.h"
#include "dtypeutl.h"
#include "llassem.h"
#include "symfun.h"

#define MXIDLEN 100
static int dataregion = 0;

static DTYPE tgt_offload_entry_type = DT_NONE;

#ifdef __cplusplus
static class ClassTgtApiCalls
{
public:
  const struct tgt_api_entry_t operator[](int off)
  {
    switch (off) {
    case TGT_API_BAD:
      return {"__INVALID_TGT_API_NAME__", -1, (DTYPE)-1};
    case TGT_API_REGISTER_LIB:
      return {"__tgt_register_lib", 0, DT_NONE};
    case TGT_API_TARGET:
      return {"__tgt_target", 0, DT_INT};
    case TGT_API_TARGET_TEAMS:
      return {"__tgt_target_teams", 0, DT_INT};
    case TGT_API_TARGET_DATA_BEGIN:
      return {"__tgt_target_data_begin", 0, DT_NONE};
    case TGT_API_TARGET_DATA_END:
      return {"__tgt_target_data_end", 0, DT_NONE};
    default:
      return {nullptr, 0, DT_NONE};
    }
  }
} tgt_api_calls;
#else
static const struct tgt_api_entry_t tgt_api_calls[] = {
    [TGT_API_BAD] = {"__INVALID_TGT_API_NAME__", -1, -1},
    [TGT_API_REGISTER_LIB] = {"__tgt_register_lib", 0, DT_NONE},
    [TGT_API_TARGET] = {"__tgt_target", 0, DT_INT},
    [TGT_API_TARGET_TEAMS] = {"__tgt_target_teams", 0, DT_INT},
    [TGT_API_TARGET_DATA_BEGIN] = {"__tgt_target_data_begin", 0, DT_NONE},
    [TGT_API_TARGET_DATA_END] = {"__tgt_target_data_end", 0, DT_NONE}};
#endif
static int
gen_null_arg()
{
  int con, ili;
  INT tmp[2];

  tmp[0] = 0;
  tmp[1] = 0;
  con = getcon(tmp, DT_INT);
  ili = ad1ili(IL_ACON, con);
  return ili;
}
/* Return ili (icon/kcon, or a loaded value) for use with mk_kmpc_api_call
 * arguments.
 */
static int
ld_sptr(SPTR sptr)
{
  ISZ_T sz = size_of(DTYPEG(sptr));

  if (STYPEG(sptr) == ST_CONST) {
    if (sz == 8)
      return ad_kcon(CONVAL1G(sptr), CONVAL2G(sptr));
    return ad_icon(CONVAL2G(sptr));
  } else {
    int nme = addnme(NT_VAR, sptr, 0, 0);
    int ili = mk_address(sptr);
    if (ILI_OPC(ili) == IL_LDA)
      nme = ILI_OPND(ili, 2);
    if (sz == 8)
      return ad3ili(IL_LDKR, ili, nme, MSZ_I8);
    else
      return ad3ili(IL_LD, ili, nme, mem_size(DTY(DTYPEG(sptr))));
  }
}

#define TGT_CHK(_api) \
  (((_api) > TGT_API_BAD && (_api) < TGT_API_N_ENTRIES) ? _api : TGT_API_BAD)

#define TGT_NAME(_api) tgt_api_calls[TGT_CHK(_api)].name
#define TGT_RET_DTYPE(_api) tgt_api_calls[TGT_CHK(_api)].ret_dtype
#define TGT_RET_ILIOPC(_api) tgt_api_calls[TGT_CHK(_api)].ret_iliopc

/* The return value is allocated and maintained locally, please do not call
 * 'free' on this, bad things will probably happen.
 *
 * Caller is responsible for calling va_end()
 *
 * This function will maintain one allocation for unique function name.
 */
static const char *
build_tgt_api_name(int tgt_api)
{
  static hashmap_t names; /* Maintained in this routine */

  if (!names)
    names = hashmap_alloc(hash_functions_strings);

  return TGT_NAME(tgt_api);
}

static int
ll_make_tgt_proto(const char *nm, int tgt_api, int argc, DTYPE *args)
{
  DTYPE ret_dtype;
  /* args contains a list of dtype.  The actual sptr of args will be create in
     ll_make_ftn_outlined_params.
   */
  const SPTR func_sptr = getsymbol(nm);

  ret_dtype = TGT_RET_DTYPE(tgt_api);

  DTYPEP(func_sptr, ret_dtype);
  SCP(func_sptr, SC_EXTERN);
  STYPEP(func_sptr, ST_PROC);
  CCSYMP(func_sptr,
         1); /* currently we make all CCSYM func varargs in Fortran. */
  CFUNCP(func_sptr, 1);
  ll_make_ftn_outlined_params(func_sptr, argc, args);
  ll_process_routine_parameters(func_sptr);
  return func_sptr;
}

/* Returns the function prototype sptr.
 * 'reg_opc'   ILI op code for return value, e.g., IL_DFRIR or 0 if void
 * 'ret_dtype' dtype representing return value
 */
static int
mk_tgt_api_call(int tgt_api, int n_args, DTYPE *arg_dtypes, int *arg_ilis)
{
  int i, ilix, altilix, gargs, garg_ilis[n_args];
  SPTR fn_sptr;
  const char *nm;
  const ILI_OP ret_opc = (ILI_OP)TGT_RET_ILIOPC(tgt_api);

  /* Create the prototype for the API call */
  nm = build_tgt_api_name(tgt_api);
  fn_sptr = (SPTR)ll_make_tgt_proto(nm, tgt_api, n_args, arg_dtypes);
  sym_is_refd(fn_sptr);

  /* Update ACC routine tables and then create the JSR */
  update_acc_with_fn(fn_sptr);
  ilix = ll_ad_outlined_func2(ret_opc, IL_JSR, fn_sptr, n_args, arg_ilis);

  /* Create the GJSR */
  for (i = n_args - 1; i >= 0; --i) /* Reverse the order */
    garg_ilis[i] = arg_ilis[n_args - 1 - i];
  gargs = ll_make_outlined_garg(n_args, garg_ilis, arg_dtypes);
  altilix = ad3ili(IL_GJSR, fn_sptr, gargs, 0);

  /* Add gjsr as an alt to the jsr */
  if (ret_opc)
    ILI_ALT(ILI_OPND(ilix, 1)) = altilix;
  else
    ILI_ALT(ilix) = altilix;

  return ilix;
}

SPTR
make_array_sptr(char *name, DTYPE atype, int arraysize)
{
  SPTR array;
  DTYPE dtype;
  array = getsymbol(name);
  STYPEP(array, ST_ARRAY);
  CCSYMP(array, 1);
  {
    ADSC *adsc;
    INT con[2] = {0, arraysize};

    dtype = get_array_dtype(1, atype);
    adsc = AD_DPTR(dtype);
    AD_LWBD(adsc, 0) = stb.i1;
    AD_UPBD(adsc, 0) = getcon(con, DT_INT);
    AD_NUMELM(adsc) = AD_UPBD(adsc, 0);
  }

  DTYPEP(array, dtype);
  SCP(array, SC_AUTO);
  return array;
} /* make_array_sptr*/

DTYPE
ll_make_struct(int count, char *name, TGT_ST_TYPE *meminfo, ISZ_T sz)
{
  DTYPE dtype;
  int i;
  SPTR mem, tag, prev_mem, first_mem;
  char sname[MXIDLEN];

  tag = SPTR_NULL;
  dtype = cg_get_type(6, TY_STRUCT, NOSYM);
  if (name) {
    sprintf(sname, "%s", name);
    tag = getsymbol(sname);
    DTYPEP(tag, dtype);
    OMPACCSTRUCTP(tag, 1);
  }

  prev_mem = first_mem = SPTR_NULL;
  for (i = 0; i < count; ++i) {
    mem = (SPTR)addnewsym(meminfo[i].name); // ???
    STYPEP(mem, ST_MEMBER);
    PAROFFSETP(mem, meminfo[i].psptr);
    DTYPEP(mem, meminfo[i].dtype);
    if (prev_mem > 0)
      SYMLKP(prev_mem, mem);
    SYMLKP(mem, NOSYM);
    PSMEMP(mem, mem);
    VARIANTP(mem, prev_mem);
    CCSYMP(mem, 1);
    ADDRESSP(mem, sz);
    SCP(mem, SC_NONE);
    if (first_mem == 0)
      first_mem = mem;
    sz += size_of(meminfo[i].dtype);
    prev_mem = mem;
  }

  DTySetAlgTy(dtype, first_mem, sz, tag, 0, 0);
  return dtype;
}

/*
 * struct __tgt_offload_entry { void*, char*, i64, i32, i32 }
 */
DTYPE
ll_make_tgt_offload_entry(char *name)
{
  TGT_ST_TYPE meminfo[] = {{"addr", DT_ADDR, 0, 0},
                           {"name", DT_ADDR, 0, 0},
                           {"size", DT_INT8, 0, 0},
                           {"flags", DT_INT, 0, 0},
                           {"reserved", DT_INT, 0, 0}};

  return ll_make_struct(5, name, meminfo, 0);
}

/*
 * struct __tgt_offload_entry { void*, void*, __tgt_offload_entry
 * *,__tgt_offload_entry * }
 */
DTYPE
ll_make_tgt_device_image(char *name, DTYPE entrytype)
{
  DTYPE dtype1, dtype2;
  dtype1 = get_type(2, TY_PTR, DT_BINT);
  dtype2 = get_type(2, TY_PTR, entrytype);
  TGT_ST_TYPE meminfo[] = {
      {"ImageStart", dtype1, 0, 0},
      {"ImageEnd", dtype1, 0, 0},
      {"EntriesBegin", dtype2, 0, 0},
      {"EntriesEnd", dtype2, 0, 0},
  };

  return ll_make_struct(4, name, meminfo, 0);
}

/*
 * struct __tgt_bin_desc { i32, __tgt_device_image*, __tgt_offload_entry
 * *,__tgt_offload_entry * }
 */
DTYPE
ll_make_tgt_bin_descriptor(char *name, DTYPE entrytype, DTYPE deviceimagetype)
{
  DTYPE dtype1, dtype2;
  dtype1 = get_type(2, TY_PTR, entrytype);
  dtype2 = get_type(2, TY_PTR, deviceimagetype);
  TGT_ST_TYPE meminfo[] = {
      {"NumDeviceImages", DT_INT8, 0, 0},
      {"DeviceImages", dtype2, 0, 0},
      {"HostEntriesBegin", dtype1, 0, 0},
      {"HostEntriesEnd", dtype1, 0, 0},
  };

  return ll_make_struct(4, name, meminfo, 0);
}

SPTR
init_tgt_target_syms(const char *kernelname)
{
  SPTR eptr1, eptr2, eptr3;
  size_t size;
  char *kernelname_, *sname_region, *sname_entry;
  size = 100 + strlen(kernelname);

  /* regionId */
  NEW(sname_region, char, size);
  strcpy(sname_region, ".openmp.offload.region.");
  strcat(sname_region, kernelname);
  eptr1 = (SPTR)addnewsym(sname_region);
  DTYPEP(eptr1, DT_BINT);
  SCP(eptr1, SC_EXTERN);
  STYPEP(eptr1, ST_VAR);
  // DINITP(eptr1,1);
  SYMLKP(eptr1, gbl.consts);
  gbl.consts = eptr1;
  OMPACCSTRUCTP(eptr1, 1);

  // device functions gets "_" in the end.
  NEW(kernelname_, char, size);
  sprintf(kernelname_, "%s_\00", kernelname);
  eptr2 = getstring(kernelname_, strlen(kernelname) + 1);
  DINITP(eptr2, 1);

  NEW(sname_entry, char, size);
  strcpy(sname_entry, ".openmp.offload.entry.");
  strcat(sname_entry, kernelname);
  eptr3 = (SPTR)addnewsym(sname_entry);
  DTYPEP(eptr3, tgt_offload_entry_type);
  SCP(eptr3, SC_EXTERN);
  STYPEP(eptr3, ST_STRUCT);
  DINITP(eptr3, 1);
  SECTP(eptr3, 1);
  WEAKP(eptr3, 1);
  OMPACCSTRUCTP(eptr3, 1);

  dinit_put(DINIT_SECT, OMP_OFFLOAD_SEC);
  dinit_put(DINIT_LOC, (ISZ_T)eptr3);
  dinit_put(DINIT_OFFSET, 0);
  dinit_put(DINIT_LABEL, eptr1);
  dinit_put(DINIT_OFFSET, 8);
  dinit_put(DINIT_LABEL, eptr2);
  gbl.saddr = 16;

  return eptr1;
}

void
init_tgt_register_syms()
{
  SPTR tptr1, tptr2, tptr3, tptr4;

  tptr1 = (SPTR)addnewsym(".omp_offloading.entries_begin");
  DTYPEP(tptr1, tgt_offload_entry_type);
  SCP(tptr1, SC_EXTERN);
  DCLDP(tptr1, 1);
  STYPEP(tptr1, ST_VAR);
  SYMLKP(tptr1, gbl.consts);
  gbl.consts = tptr1;
  OMPACCRTP(tptr1, 1);

  tptr2 = (SPTR)addnewsym(".omp_offloading.entries_end");
  DTYPEP(tptr2, tgt_offload_entry_type);
  SCP(tptr2, SC_EXTERN);
  DCLDP(tptr2, 1);
  STYPEP(tptr2, ST_VAR);
  SYMLKP(tptr2, gbl.consts);
  gbl.consts = tptr2;
  OMPACCRTP(tptr2, 1);

  tptr3 = (SPTR)addnewsym(".omp_offloading.img_start.nvptx64-nvidia-cuda");
  DTYPEP(tptr3, DT_BINT);
  SCP(tptr3, SC_EXTERN);
  STYPEP(tptr3, ST_VAR);
  SYMLKP(tptr3, gbl.consts);
  gbl.consts = tptr3;
  OMPACCRTP(tptr3, 1);

  tptr4 = (SPTR)addnewsym(".omp_offloading.img_end.nvptx64-nvidia-cuda");
  DTYPEP(tptr4, DT_BINT);
  SCP(tptr4, SC_EXTERN);
  DCLDP(tptr4, 1);
  STYPEP(tptr4, ST_VAR);
  SYMLKP(tptr4, gbl.consts);
  gbl.consts = tptr4;
  OMPACCRTP(tptr4, 1);
}

int
ll_make_tgt_register_lib()
{
  SPTR sptr;
  DTYPE dtype_bindesc, dtype_entry, dtype_devimage, dtype_pofbindesc;

  dtype_entry = tgt_offload_entry_type;
  dtype_devimage = ll_make_tgt_device_image("__tgt_device_image", dtype_entry);
  dtype_bindesc =
      ll_make_tgt_bin_descriptor("__tgt_bin_desc", dtype_entry, dtype_devimage);

  sptr = (SPTR)addnewsym(".omp_offloading.descriptor");
  STYPEP(sptr, ST_STRUCT);
  SCP(sptr, SC_EXTERN);
  REFP(sptr, 1);
  DTYPEP(sptr, dtype_bindesc);

  dtype_pofbindesc = get_type(2, TY_PTR, dtype_bindesc);
  int args[1];
  DTYPE arg_types[1] = {dtype_pofbindesc};
  args[0] = ad_acon(sptr, 0);
  return mk_tgt_api_call(TGT_API_REGISTER_LIB, 1, arg_types, args);
}

int
ll_make_tgt_register_lib2()
{
  SPTR tptr1, tptr2, tptr3, tptr4, tptr;
  SPTR sptr, sptr2;
  int i, ilix, nme, offset, addr;
  DTYPE dtype_entry, dtype_devimage, dtype_bindesc, dtype_pofbindesc;

  init_tgt_register_syms();

  for (tptr = gbl.consts; tptr > NOSYM; tptr = SYMLKG(tptr)) {
    if (OMPACCRTG(tptr)) {
      tptr4 = tptr;
      tptr3 = SYMLKG(tptr4);
      tptr2 = SYMLKG(tptr3);
      tptr1 = SYMLKG(tptr2);
      break;
    }
  }
  assert(!tptr || !tptr2 || !tptr3 || !tptr4,
         "OpenMP Offload structures are not found.", 0, ERR_Fatal);

  dtype_entry =
      tgt_offload_entry_type; // ll_make_tgt_offload_entry("__tgt_offload_entry");
  dtype_devimage = ll_make_tgt_device_image("__tgt_device_image", dtype_entry);
  dtype_bindesc =
      ll_make_tgt_bin_descriptor("__tgt_bin_desc", dtype_entry, dtype_devimage);

  sptr = (SPTR)addnewsym(".omp_offloading.device_images");
  STYPEP(sptr, ST_STRUCT);
  SCP(sptr, SC_LOCAL);
  REFP(sptr, 1);
  DTYPEP(sptr, dtype_devimage);
  nme = addnme(NT_VAR, sptr, 0, 0);

  offset = 0;
  i = DTY(DTYPE(dtype_devimage + 1));
  addr = ad_acon(sptr, offset);
  ilix =
      ad4ili(IL_ST, ad_acon(tptr3, 0), addr,
             addnme(NT_MEM, (SPTR)PSMEMG(i), nme, 0), mem_size(DTY(DTYPEG(i))));
  offset += size_of(DTYPEG(i));
  chk_block(ilix);

  i = SYMLKG(i);
  addr = ad_acon(sptr, offset);
  ilix =
      ad4ili(IL_ST, ad_acon(tptr4, 0), addr,
             addnme(NT_MEM, (SPTR)PSMEMG(i), nme, 0), mem_size(DTY(DTYPEG(i))));
  offset += size_of(DTYPEG(i));
  chk_block(ilix);

  i = SYMLKG(i);
  addr = ad_acon(sptr, offset);
  ilix =
      ad4ili(IL_ST, ad_acon(tptr1, 0), addr,
             addnme(NT_MEM, (SPTR)PSMEMG(i), nme, 0), mem_size(DTY(DTYPEG(i))));
  offset += size_of(DTYPEG(i));
  chk_block(ilix);

  i = SYMLKG(i);
  addr = ad_acon(sptr, offset);
  ilix =
      ad4ili(IL_ST, ad_acon(tptr2, 0), addr,
             addnme(NT_MEM, (SPTR)PSMEMG(i), nme, 0), mem_size(DTY(DTYPEG(i))));
  chk_block(ilix);

  sptr2 = (SPTR)addnewsym(".omp_offloading.descriptor");
  STYPEP(sptr2, ST_STRUCT);
  SCP(sptr2, SC_LOCAL);
  REFP(sptr2, 1);
  DTYPEP(sptr2, dtype_bindesc);
  nme = addnme(NT_VAR, sptr2, 0, 0);

  offset = 0;
  i = DTY(DTYPE(dtype_bindesc + 1));
  addr = ad_acon(sptr2, offset);
  ilix =
      ad4ili(IL_ST, ad_icon(1), addr, addnme(NT_MEM, (SPTR)PSMEMG(i), nme, 0),
             mem_size(DTY(DTYPEG(i))));
  offset += size_of(DTYPEG(i));
  chk_block(ilix);
  i = SYMLKG(i);
  addr = ad_acon(sptr2, offset);
  ilix =
      ad4ili(IL_ST, ad_acon(sptr, 0), addr,
             addnme(NT_MEM, (SPTR)PSMEMG(i), nme, 0), mem_size(DTY(DTYPEG(i))));
  offset += size_of(DTYPEG(i));
  chk_block(ilix);
  i = SYMLKG(i);
  addr = ad_acon(sptr2, offset);
  ilix =
      ad4ili(IL_ST, ad_acon(tptr1, 0), addr,
             addnme(NT_MEM, (SPTR)PSMEMG(i), nme, 0), mem_size(DTY(DTYPEG(i))));
  offset += size_of(DTYPEG(i));
  chk_block(ilix);
  i = SYMLKG(i);
  addr = ad_acon(sptr2, offset);
  ilix =
      ad4ili(IL_ST, ad_acon(tptr2, 0), addr,
             addnme(NT_MEM, (SPTR)PSMEMG(i), nme, 0), mem_size(DTY(DTYPEG(i))));
  offset += size_of(DTYPEG(i));
  chk_block(ilix);

  dtype_pofbindesc = get_type(2, TY_PTR, dtype_bindesc);
  int args[1];
  DTYPE arg_types[1] = {dtype_pofbindesc};
  args[0] = ad_acon(sptr2, 0);
  return mk_tgt_api_call(TGT_API_REGISTER_LIB, 1, arg_types, args);
}

void
tgt_target_fill_params(SPTR arg_base_sptr, SPTR arg_size_sptr, SPTR args_sptr,
                       SPTR args_maptypes_sptr, OMPACCEL_TINFO *targetinfo)
{
  int param_ili, i, j, index, nme_args, nme_size, nme_base, nme_types, nme, ili,
      size, sili;
  DTYPE dtype;
  SPTR param_sptr, midnum_sptr;
  ADSC *ad;
  LOGICAL ismidnum;
  /* fill the arrays */
  /* Build the list: (size, sptr) pairs. */
  nme_args = addnme(NT_VAR, args_sptr, 0, 0);
  nme_size = addnme(NT_VAR, arg_size_sptr, 0, 0);
  nme_base = addnme(NT_VAR, arg_base_sptr, 0, 0);
  nme_types = addnme(NT_VAR, args_maptypes_sptr, 0, 0);

  for (i = 0; i < targetinfo->n_symbols; ++i) {
    param_sptr = targetinfo->symbols[i].host_sym;

    /* Check whether it is a midnum */
    ismidnum = FALSE;
    for (j = 0; j < targetinfo->n_quiet_symbols; ++j) {
      int midnum_sptr = MIDNUMG(targetinfo->quiet_symbols[j].host_sym);
      if (midnum_sptr == param_sptr || HASHLKG(midnum_sptr) == param_sptr) {
        ismidnum = TRUE;
        param_sptr = targetinfo->quiet_symbols[j].host_sym;
        break;
      }
    }

    dtype = DTYPEG(param_sptr);

    if (ismidnum) {
      midnum_sptr = MIDNUMG(param_sptr);
      param_ili = ad3ili(IL_LDA, ad_acon(midnum_sptr, 0),
                         addnme(NT_VAR, midnum_sptr, (INT)0, 0), MSZ_PTR);

      /* assign arg */
      nme = add_arrnme(NT_ARR, args_sptr, nme_args, 0, ad_icon(i), FALSE);
      ili = ad4ili(IL_STA, param_ili, ad_acon(args_sptr, i * TARGET_PTRSIZE),
                   nme, MSZ_I8);
      chk_block(ili);

      /* assign base */
      nme = add_arrnme(NT_ARR, arg_base_sptr, nme_base, 0, ad_icon(i), FALSE);
      ili = ad4ili(IL_STA, param_ili,
                   ad_acon(arg_base_sptr, i * TARGET_PTRSIZE), nme, MSZ_I8);
      chk_block(ili);
    } else if ((STYPEG(param_sptr) == ST_ARRAY) ||
               PASSBYVALG(targetinfo->symbols[i].device_sym) == 0) {
      param_ili = ad_acon(param_sptr, 0);
      /* assign arg */
      nme = add_arrnme(NT_ARR, args_sptr, nme_args, 0, ad_icon(i), FALSE);
      ili = ad3ili(IL_STA, param_ili, ad_acon(args_sptr, i * TARGET_PTRSIZE),
                   nme);
      chk_block(ili);
      /* assign base */
      // todo ompaccel find the real base. For now, I assume users request
      // entire array mapping
      nme = add_arrnme(NT_ARR, arg_base_sptr, nme_base, 0, ad_icon(i), FALSE);

      ili = ad3ili(IL_STA, param_ili,
                   ad_acon(arg_base_sptr, i * TARGET_PTRSIZE), nme);
      chk_block(ili);
    } else {
      param_ili = ld_sptr(param_sptr);

      /* assign arg */
      nme = add_arrnme(NT_ARR, args_sptr, nme_args, 0, ad_icon(i), FALSE);
      ili = mk_ompaccel_store(param_ili, DTYPEG(param_sptr), nme,
                              ad_acon(args_sptr, i * TARGET_PTRSIZE));
      chk_block(ili);

      /* assign base */
      nme = add_arrnme(NT_ARR, arg_base_sptr, nme_base, 0, ad_icon(i), FALSE);
      ili = mk_ompaccel_store(param_ili, DTYPEG(param_sptr), nme,
                              ad_acon(arg_base_sptr, i * TARGET_PTRSIZE));
      chk_block(ili);
    }

    /* assign map type */
    int map_type = 0;
    if (ismidnum) {
      if (targetinfo->quiet_symbols[j].map_type == 0) {
        targetinfo->quiet_symbols[j].map_type =
            OMP_TGT_MAPTYPE_IMPLICIT | OMP_TGT_MAPTYPE_TARGET_PARAM;
        if (DTY(dtype) != TY_ARRAY)
          targetinfo->quiet_symbols[j].map_type |= OMP_TGT_MAPTYPE_LITERAL;
      }
      map_type = targetinfo->quiet_symbols[j].map_type;
      targetinfo->symbols[i].map_type = map_type;
    } else {
      if (targetinfo->symbols[i].map_type == 0) {
        targetinfo->symbols[i].map_type =
            OMP_TGT_MAPTYPE_IMPLICIT | OMP_TGT_MAPTYPE_TARGET_PARAM;
        if (DTY(dtype) != TY_ARRAY)
          targetinfo->symbols[i].map_type |= OMP_TGT_MAPTYPE_LITERAL;
      }
      map_type = targetinfo->symbols[i].map_type;
    }
    nme =
        add_arrnme(NT_ARR, args_maptypes_sptr, nme_types, 0, ad_icon(i), FALSE);
    ili = ad4ili(IL_ST, ad_icon(map_type),
                 ad_acon(args_maptypes_sptr, i * TARGET_PTRSIZE), nme, MSZ_I8);
    chk_block(ili);

    /* assign size */
    if (DTY(dtype) == TY_ARRAY && (map_type & OMP_TGT_MAPTYPE_IMPLICIT)) {
      size = ad_icon(0);
    } else if (DTY(dtype) == TY_ARRAY) {
      ad = AD_DPTR(dtype);
      if (SCG(param_sptr) == SC_STATIC) {
        int j, numdim = AD_NUMDIM(ad);
        size = 1;
        for (j = 0; j < numdim; ++j) {
          size = size * CONVAL2G(AD_UPBD(ad, j)) - CONVAL2G(AD_LWBD(ad, j)) + 1;
        }
        size = size_of(DTYPE(dtype + 1)) * size;
        size = ad_icon(size);
      } else {
        int numdim = AD_NUMDIM(ad);
        int j;
        size = ad_icon(1);
        // todo ompaccel we do not support partial arrays here.
        for (j = 0; j < numdim; ++j) {
          if (AD_UPBD(ad, j) != 0) {
            sili = ad2ili(IL_ISUB, ld_sptr((SPTR) AD_UPBD(ad, j)), ld_sptr((SPTR) AD_LWBD(ad, j)));
            sili = ad2ili(IL_IADD, sili, ad_icon(1));
          } else
            sili = ad2ili(IL_IADD, ad_icon(0), ad_icon(1));
          size = ad2ili(IL_IMUL, size, sili);
        }
        size = ad2ili(IL_IMUL, size, ad_icon(size_of(DTYPE(dtype + 1))));
      }
    } else {
      size = ad_icon(size_of(dtype));
    }
    nme = add_arrnme(NT_ARR, arg_size_sptr, nme_size, 0, ad_icon(i), FALSE);
    ili = ad4ili(IL_ST, size, ad_acon(arg_size_sptr, i * TARGET_PTRSIZE), nme,
                 TARGET_PTRSIZE == 8 ? MSZ_I8 : MSZ_WORD);
    chk_block(ili);
  }
}

/* Dump the values being stored in the uplevel argument */
static void
dumpUplevel(int uplevel_sptr)
{
  int i;
  FILE *fp = gbl.dbgfil ? gbl.dbgfil : stdout;

  fprintf(fp, "********* UPLEVEL Struct *********\n");
  for (i = DTY(DTYPE(DTYPEG(uplevel_sptr) + 1)); i > NOSYM; i = SYMLKG(i))
    fprintf(fp, "==> %s %s\n", SYMNAME(i), stb.tynames[DTY(DTYPEG(i))]);
  fprintf(fp, "**********\n\n");
}

void
change_target_func_smbols(int outlined_func_sptr, int stblk_sptr)
{
  int dpdsc, paramct, i, j, block_sptr, target_sptr;
  const LLUplevel *uplevel;

  // perhaps it is an empty target region
  if (PARSYMSG(stblk_sptr) == 0) {
    return;
  }

  uplevel = llmp_get_uplevel(stblk_sptr);
  paramct = PARAMCTG(outlined_func_sptr);

  for (i = 0; i < uplevel->vals_count; ++i) {
    block_sptr = uplevel->vals[i];
    if (!block_sptr)
      continue;
    dpdsc = DPDSCG(outlined_func_sptr);
    for (j = 0; j < paramct; ++j, ++dpdsc) {
      target_sptr = aux.dpdsc_base[dpdsc];
      if (strncmp(&SYMNAME(target_sptr)[4], SYMNAME(block_sptr),
                  strlen(SYMNAME(block_sptr))) == 0) {
        uplevel->vals[i] = target_sptr;
        break;
      }
    }
  }
}

int
ll_make_tgt_target(SPTR outlined_func_sptr, int64_t device_id, SPTR stblk_sptr)
{
  SPTR sptr, arg_base_sptr, arg_size_sptr, args_sptr, args_maptypes_sptr;
  char *name, *rname;
  OMPACCEL_TINFO *targetinfo;

  rname = SYMNAME(outlined_func_sptr);
  NEW(name, char, 100);

  targetinfo = ompaccel_tinfo_get(outlined_func_sptr);

  sptr = init_tgt_target_syms(rname);
  if (targetinfo->n_symbols == 0) {
    int locargs[7];
    DTYPE locarg_types[] = {DT_INT8, DT_ADDR, DT_INT, DT_ADDR,
                            DT_ADDR, DT_ADDR, DT_ADDR};
    locargs[6] = ad_icon(device_id);
    locargs[5] = ad_acon(sptr, 0);
    locargs[4] = ad_icon(targetinfo->n_symbols);
    locargs[3] = gen_null_arg();
    locargs[2] = gen_null_arg();
    locargs[1] = gen_null_arg();
    locargs[0] = gen_null_arg();
    // call the RT
    int call_ili = mk_tgt_api_call(TGT_API_TARGET, 7, locarg_types, locargs);
    return call_ili;
  } else {
    sprintf(name, "%s_base", rname);
    arg_base_sptr = make_array_sptr(name, DT_CPTR, targetinfo->n_symbols);
    sprintf(name, "%s_size", rname);
    arg_size_sptr = make_array_sptr(name, DT_INT8, targetinfo->n_symbols);
    sprintf(name, "%s_args", rname);
    args_sptr = make_array_sptr(name, DT_CPTR, targetinfo->n_symbols);
    sprintf(name, "%s_type", rname);
    args_maptypes_sptr = make_array_sptr(name, DT_INT8, targetinfo->n_symbols);

    tgt_target_fill_params(arg_base_sptr, arg_size_sptr, args_sptr,
                           args_maptypes_sptr, targetinfo);

    // prepare argument for tgt target
    int locargs[7];
    DTYPE locarg_types[] = {DT_INT8, DT_ADDR, DT_INT, DT_ADDR,
                            DT_ADDR, DT_ADDR, DT_ADDR};
    locargs[6] = ad_icon(device_id);
    locargs[5] = ad_acon(sptr, 0);
    locargs[4] = ad_icon(targetinfo->n_symbols);
    locargs[3] = ad_acon(arg_base_sptr, 0);
    locargs[2] = ad_acon(args_sptr, 0);
    locargs[1] = ad_acon(arg_size_sptr, 0);
    locargs[0] = ad_acon(args_maptypes_sptr, 0);

    change_target_func_smbols(outlined_func_sptr, stblk_sptr);

    // call the RT
    int call_ili = mk_tgt_api_call(TGT_API_TARGET, 7, locarg_types, locargs);

    return call_ili;
  }
}

int
ll_make_tgt_target_teams(SPTR outlined_func_sptr, int64_t device_id,
                         SPTR stblk_sptr, int32_t num_teams,
                         int32_t thread_limit)
{
  SPTR sptr, arg_base_sptr, arg_size_sptr, args_sptr, args_maptypes_sptr;
  char *name, *rname;
  OMPACCEL_TINFO *targetinfo = ompaccel_tinfo_get(outlined_func_sptr);
  int nargs = targetinfo->n_symbols;
  rname = SYMNAME(outlined_func_sptr);
  NEW(name, char, 100);
  sptr = init_tgt_target_syms(rname);

  sprintf(name, "%s_base", rname);
  arg_base_sptr = make_array_sptr(name, DT_CPTR, nargs);
  sprintf(name, "%s_size", rname);
  arg_size_sptr = make_array_sptr(name, DT_INT8, nargs);
  sprintf(name, "%s_args", rname);
  args_sptr = make_array_sptr(name, DT_CPTR, nargs);
  sprintf(name, "%s_type", rname);
  args_maptypes_sptr = make_array_sptr(name, DT_INT8, nargs);

  tgt_target_fill_params(arg_base_sptr, arg_size_sptr, args_sptr,
                         args_maptypes_sptr, targetinfo);

  // prepare argument for tgt target
  int locargs[9];
  DTYPE locarg_types[] = {DT_INT8, DT_ADDR, DT_INT, DT_ADDR, DT_ADDR,
                          DT_ADDR, DT_ADDR, DT_INT, DT_INT};
  locargs[8] = ad_icon(device_id);
  locargs[7] = ad_acon(sptr, 0);
  locargs[6] = ad_icon(nargs);
  locargs[5] = ad_acon(arg_base_sptr, 0);
  locargs[4] = ad_acon(args_sptr, 0);
  locargs[3] = ad_acon(arg_size_sptr, 0);
  locargs[2] = ad_acon(args_maptypes_sptr, 0);
  locargs[1] = ad_icon(num_teams);
  locargs[0] = ad_icon(thread_limit);

  change_target_func_smbols(outlined_func_sptr, stblk_sptr);

  // call the RT
  int call_ili =
      mk_tgt_api_call(TGT_API_TARGET_TEAMS, 9, locarg_types, locargs);

  return call_ili;
}

int
ll_make_tgt_target_data_begin(int device_id, OMPACCEL_TINFO *targetinfo)
{
  int call_ili, nargs;
  SPTR arg_base_sptr, args_sptr, arg_size_sptr, args_maptypes_sptr;
  char name[12];

  int locargs[6];
  DTYPE locarg_types[] = {DT_INT8, DT_INT, DT_ADDR, DT_ADDR, DT_ADDR, DT_ADDR};

  if (targetinfo == nullptr) {
    interr("Map item list is not found", 0, ERR_Fatal);
  }
  nargs = targetinfo->n_symbols;

  sprintf(name, "edata%d_base", dataregion);
  arg_base_sptr = make_array_sptr(name, DT_CPTR, nargs);
  sprintf(name, "edata%d_size", dataregion);
  arg_size_sptr = make_array_sptr(name, DT_INT8, nargs);
  sprintf(name, "edata%d_args", dataregion);
  args_sptr = make_array_sptr(name, DT_CPTR, nargs);
  sprintf(name, "edata%d_type", dataregion);
  args_maptypes_sptr = make_array_sptr(name, DT_INT8, nargs);
  dataregion++;

  tgt_target_fill_params(arg_base_sptr, arg_size_sptr, args_sptr,
                         args_maptypes_sptr, targetinfo);

  locargs[5] = ad_icon(device_id);
  locargs[4] = ad_icon(nargs);
  locargs[3] = ad_acon(arg_base_sptr, 0);
  locargs[2] = ad_acon(args_sptr, 0);
  locargs[1] = ad_acon(arg_size_sptr, 0);
  locargs[0] = ad_acon(args_maptypes_sptr, 0);

  // call the RT
  call_ili =
      mk_tgt_api_call(TGT_API_TARGET_DATA_BEGIN, 6, locarg_types, locargs);

  return call_ili;
}

int
ll_make_tgt_target_data_end(int device_id, OMPACCEL_TINFO *targetinfo)
{
  int call_ili, nargs;
  SPTR arg_base_sptr, args_sptr, arg_size_sptr, args_maptypes_sptr;
  char name[12];

  int locargs[6];
  DTYPE
  locarg_types[] = {DT_INT8, DT_INT, DT_ADDR, DT_ADDR, DT_ADDR, DT_ADDR};

  if (targetinfo == nullptr) {
    interr("Map item list is not found", 0, ERR_Fatal);
  }

  nargs = targetinfo->n_symbols;

  sprintf(name, "xdata%d_base", dataregion);
  arg_base_sptr = make_array_sptr(name, DT_CPTR, nargs);
  sprintf(name, "xdata%d_size", dataregion);
  arg_size_sptr = make_array_sptr(name, DT_INT8, nargs);
  sprintf(name, "xdata%d_args", dataregion);
  args_sptr = make_array_sptr(name, DT_CPTR, nargs);
  sprintf(name, "xdata%d_type", dataregion);
  args_maptypes_sptr = make_array_sptr(name, DT_INT8, nargs);
  dataregion++;

  tgt_target_fill_params(arg_base_sptr, arg_size_sptr, args_sptr,
                         args_maptypes_sptr, targetinfo);

  locargs[5] = ad_icon(device_id);
  locargs[4] = ad_icon(nargs);
  locargs[3] = ad_acon(arg_base_sptr, 0);
  locargs[2] = ad_acon(args_sptr, 0);
  locargs[1] = ad_acon(arg_size_sptr, 0);
  locargs[0] = ad_acon(args_maptypes_sptr, 0);

  // call the RT
  call_ili = mk_tgt_api_call(TGT_API_TARGET_DATA_END, 6, locarg_types, locargs);

  return call_ili;
}

void
init_tgtutil()
{
  tgt_offload_entry_type = ll_make_tgt_offload_entry("__tgt_offload_entry_");
}

#endif
