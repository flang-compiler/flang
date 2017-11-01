#
# Copyright (c) 2017, NVIDIA CORPORATION.  All rights reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

enable_language(C ASM Fortran) # Enable assembly and Fortran

SET(ASM_OPTIONS "-DLINUX_ELF")
SET(CMAKE_ASM_FLAGS "${CFLAGS} ${ASM_OPTIONS}" )
if (NOT MSVC)
SET(CMAKE_SHARED_LINKER_FLAGS "-no-flang-libs")
endif()

# We are using Fortran driver to build this library with fresh compiler
# components, so point its binary directory to the build directory to pick up
# flang* executables
SET(CMAKE_Fortran_FLAGS "-B ${LLVM_RUNTIME_OUTPUT_INTDIR} ${CMAKE_Fortran_FLAGS}")

SET(FTN_INTRINSICS
  abort3f.c
  access3f.c
  acos.c
  acosd.c
  alarm3f.c
  amod.c
  asin.c
  asind.c
  atan2.c
  atan2d.c
  atan.c
  atand.c
  besj03f.c
  besj13f.c
  besjn3f.c
  besy03f.c
  besy13f.c
  besyn3f.c
  cabs.c
  ccos.c
  cdabs.c
  cdcos.c
  cddivd.c
  cdexp.c
  cdivr.c
  cdlog.c
  cdpowcd.c
  cdpowi.c
  cdpowk.c
  cdsin.c
  cdsqrt.c
  cexp.c
  chdir3f.c
  chmod3f.c
  clog.c
  commitqq3f.c
  cos.c
  cosd.c
  cosh.c
  cpowc.c
  cpowi.c
  cpowk.c
  csin.c
  csqrt.c
  ctime3f.c
  cvt.c
  dacos.c
  dacosd.c
  dasin.c
  dasind.c
  datan2.c
  datan2d.c
  datan.c
  datand.c
  date3f.c
  dbesj03f.c
  dbesj13f.c
  dbesjn3f.c
  dbesy03f.c
  dbesy13f.c
  dbesyn3f.c
  dcos.c
  dcosd.c
  dcosh.c
  delfilesqq3f.c
  derf3f.c
  derfc3f.c
  dexp.c
  dlog10.c
  dlog.c
  dmod.c
  drandm3f.c
  dsecnds3f.c
  dsin.c
  dsind.c
  dsinh.c
  dsqrt.c
  dtan.c
  dtand.c
  dtanh.c
  dtime3f.c
  erf3f.c
  erfc3f.c
  etime3f.c
  exit3f.c
  exp.c
  fdate3f.c
  fgetc3f.c
  findfileqq3f.c
  flush3f.c
  fork3f.c
  fpcvt.c
  fputc3f.c
  free3f.c
  fseek3f.c
  fseek643f.c
  fstat3f.c
  fstat643f.c
  fsync3f.c
  ftell3f.c
  ftell643f.c
  ftn64.c
  ftnbitsup.c
  ftncharsup.c
  ftnhdr.F
  ftni64.c
  ftni64.h
  ftni64bitsup.c
  ftni64misc.c
  ftnmiscsup.c
  ftnncharsup.c
  fullpathqq3f.c
  gather.c
  gathscat.c
  gerror3f.c
  getarg3f.c
  getc3f.c
  getcwd3f.c
  getdat3f.c
  getdrivedirqq3f.c
  getenv3f.c
  getenvqq3f.c
  getfd3f.c
  getfileinfoqq3f.c
  getfileinfoqqi83f.c
  getgid3f.c
  getlog3f.c
  getpid3f.c
  gettim3f.c
  getuid3f.c
  getvolinfo3f.c
  gmtime3f.c
  hostnm3f.c
  i1shft.c
  i2powi.c
  idate3f.c
  ierrno3f.c
  iishft.c
  ileadz.c
  ileadzi.c
  intrin.c
  ioinit3f.c
  ipopcnt.c
  ipopcnti.c
  ipoppar.c
  ipoppari.c
  irand3f.c
  irandm3f.c
  isatty3f.c
  isinfd3f.c
  isinff3f.c
  isnand3f.c
  isnanf3f.c
  itime3f.c
  kabs.c
  kidim.c
  kill3f.c
  kisign.c
  kleadz.c
  kmax.c
  kmin.c
  kpopcnt.c
  kpoppar.c
  link3f.c
  lnblnk3f.c
  loc3f.c
  long3f.c
  lstat3f.c
  lstat643f.c
  ltime3f.c
  malloc3f.c
  mclock3f.c
  mod.c
  mvbits3f.c
  nargs3f.c
  omp_lib.F95
  outstr3f.c
  packtimeqq3f.c
  perror3f.c
  putc3f.c
  putenv3f.c
  pxffileno3f.c
  qsort3f.c
  rand3f.c
  random3f.c
  range3f.c
  red.c
  red_all.c
  red_any.c
  red_count.c
  red_iany.c 
  red_findloc.c
  red_maxloc.c
  red_minloc.c
  red_maxval.c
  red_minval.c
  red_sum.c
  reduct.c
  rename3f.c
  renamefileqq3f.c
  rindex3f.c
  rtc3f.c
  runqq3f.c
  scatter.c
  scatter_maxval.c
  scatter_minval.c
  secnds3f.c
  setenvqq3f.c
  setfileaccessqq3f.c
  setfiletimeqq3f.c
  setvbuf3f.c
  short3f.c
  signal3f.c
  signalqq3f.c
  sign.c
  sin.c
  sind.c
  sinh.c
  sleep3f.c
  sleepqq3f.c
  splitpathqq3f.c
  sqrt.c
  srand3f.c
  stat3f.c
  stat643f.c
  stime3f.c
  symlnk3f.c
  system3f.c
  systemqq3f.c
  tan.c
  tand.c
  tanh.c
  time3f.c
  timef3f.c
  times3f.c
  ttynam3f.c
  unlink3f.c
  unpacktimeqq3f.c
  utils3f.c
  wait3f.c
  )

SET(FTN_SUPPORT
  bcopy.c
  bcopys.c
  buffer.c
  chn1t1.c
  chn1tn.c
  chnbcst_loop.c
  chnmerge.c
  const.c
  cprof2.c
  cprof.c
  curdir.c
  genlist.c
  hand.c
  heapinit.c
  map.c
  stat.c
  trace2.c
  trace.c
  usrio_smp.c
  xfer_heap_dum.c
  aligned.c
  allo.c
  assign.c
  async.c
  atol.c
  backspace.c
  close.c
  cnfg.c
  comm.c
  copy.c
  cplxf.c
  csect.c
  cshift.c
  dbug.c
  defs.c
  desc.c
  dist.c
  dsign.c
  dynam.c
  encodefmt.c
  endfile.c
  entry.c
  eoshift.c
  error.c
  exch.c
  fill.c
  flush.c
  fmtconv.c
  fmtgetnum.c
  fmtread.c
  fmtwrite.c
  format-double.c
  fpcvt.c
  ftn.c
  ftnexit.c
  gather_cmplx16.F95
  gather_cmplx8.F95
  gather_real4.F95
  gather_real8.F95
  getarg.c
  grad.c
  descIntrins.c
  descFioUtil.c
  descRW.c
  ieee_arithmetic.F95
  ieee_exceptions.F95
  ieee_features.F95
  initpar.c
  inquire.c
  iso_c_bind.F95
  iso_fortran_env.f90
  ldread.c
  ldwrite.c
  linux_dummy.c
  malloc.c
  mget.c
  misc.c
  miscsup_com.c
  mmul.c
  mmcmplx16.c
  mmcmplx8.c
  mmreal4.c
  mmreal8.c
  mmulcplx16.c
  mmul_cplx16contmxm.F95
  mmul_cplx16contmxv.F95
  mmul_cplx16contvxm.F95
  mmul_cplx16str1.F95
  mmul_cplx16str1_t.F95
  mmulcplx16_t.c
  mmulcplx8.c
  mmul_cplx8contmxm.F95
  mmul_cplx8contmxv.F95
  mmul_cplx8contvxm.F95
  mmul_cplx8str1.F95
  mmul_cplx8str1_t.F95
  mmulcplx8_t.c
  mmulint1.c
  mmul_int1contmxm.F95
  mmul_int1contmxv.F95
  mmul_int1contvxm.F95
  mmul_int1str1.F95
  mmulint2.c
  mmul_int2contmxm.F95
  mmul_int2contmxv.F95
  mmul_int2contvxm.F95
  mmul_int2str1.F95
  mmulint4.c
  mmul_int4contmxm.F95
  mmul_int4contmxv.F95
  mmul_int4contvxm.F95
  mmul_int4str1.F95
  mmulint8.c
  mmul_int8contmxm.F95
  mmul_int8contmxv.F95
  mmul_int8contvxm.F95
  mmul_int8str1.F95
  mmullog1.c
  mmul_log1contmxm.F95
  mmul_log1contmxv.F95
  mmul_log1contvxm.F95
  mmullog2.c
  mmul_log2contmxm.F95
  mmul_log2contmxv.F95
  mmul_log2contvxm.F95
  mmullog4.c
  mmul_log4contmxm.F95
  mmul_log4contmxv.F95
  mmul_log4contvxm.F95
  mmullog8.c
  mmul_log8contmxm.F95
  mmul_log8contmxv.F95
  mmul_log8contvxm.F95
  mmulreal4.c
  mmul_real4contmxm.F95
  mmul_real4contmxv.F95
  mmul_real4contvxm.F95
  mmul_real4str1.F95
  mmul_real4str1_t.F95
  mmulreal4_t.c
  mmulreal8.c
  mmul_real8contmxm.F95
  mmul_real8contmxv.F95
  mmul_real8contvxm.F95
  mmul_real8str1.F95
  mmul_real8str1_t.F95
  mmulreal8_t.c
  mnaxnb_cmplx16.F95
  mnaxnb_cmplx8.F95
  mnaxnb_real4.F95
  mnaxnb_real8.F95
  mnaxtb_cmplx16.F95
  mnaxtb_cmplx8.F95
  mnaxtb_real4.F95
  mnaxtb_real8.F95
  mtaxnb_cmplx16.F95
  mtaxnb_cmplx8.F95
  mtaxnb_real4.F95
  mtaxnb_real8.F95
  mtaxtb_cmplx16.F95
  mtaxtb_cmplx8.F95
  mtaxtb_real4.F95
  mtaxtb_real8.F95
  mvmul_cmplx16.F95
  mvmul_cmplx8.F95
  mvmul_real4.F95
  mvmul_real8.F95
  nmlread.c
  nmlwrite.c
  nmlutil.c
  olap.c
  open.c
  pack.c
  fiodf.c
  ptr.c
  query.c
  rdst.c
  reshape.c
  rewind.c
  rnum.c
  rw.c
  scalar_copy.c
  scal.c
  spread.c
  stat_linux.c
  transfer.c
  transpose_cmplx16.F95
  transpose_cmplx8.F95
  transpose_real4.F95
  transpose_real8.F95
  type.c
  unf.c
  util.c
  utils.c
  utilsi64.c
  version.c
  vmmul_cmplx16.F95
  vmmul_cmplx8.F95
  vmmul_real4.F95
  vmmul_real8.F95
  wait.c
  xfer.c
  init.c
  xfer_rpm1.c
  )

add_flang_library(flang_static
  ${FTN_INTRINSICS}
  ${FTN_SUPPORT}
  ${SHARED_SOURCES}
  )
  
if (MSVC)
set_property(TARGET flang_static PROPERTY OUTPUT_NAME libflang)
else()
set_property(TARGET flang_static PROPERTY OUTPUT_NAME flang)
endif()

set(SHARED_LIBRARY TRUE)
add_flang_library(flang_shared
  ${FTN_INTRINSICS}
  ${FTN_SUPPORT}
  ${SHARED_SOURCES}
  )
set_property(TARGET flang_shared PROPERTY OUTPUT_NAME flang)
target_link_libraries(flang_shared flangrti_shared)
# Resolve symbols against libm and librt
if (NOT MSVC)
target_link_libraries(flang_shared rt m)
else()
  set_target_properties(flang_shared PROPERTIES WINDOWS_EXPORT_ALL_SYMBOLS TRUE)
endif()

set(SHARED_LIBRARY FALSE)

set_property(
  SOURCE ${FTN_INSTRINICS} ${SHARED_SOURCES}
  PROPERTY COMPILE_DEFINITIONS 
  PGF90
  KANJI
  INT32PTR64
  TM_I8
  HAVE_LONG_LONG_INT
 )

if(WIN32)
set_property(
 SOURCE ${FTN_SUPPORT} 
 PROPERTY COMPILE_DEFINITIONS 
  TARGET_WIN_X8664
  INT32PTR64
  TM_I8
  HAVE_LONG_LONG_INT
 )
else()
set_property(
 SOURCE ${FTN_SUPPORT} 
 PROPERTY COMPILE_DEFINITIONS 
  TARGET_LINUX_X8664
  INT32PTR64
  TM_I8
  HAVE_LONG_LONG_INT
 )
endif()

set_property(
 SOURCE initpar.c
 PROPERTY COMPILE_DEFINITIONS
 PG_PIC
 )

## CMake does not handle module dependencies between Fortran files,
## we need to help it

# State the module that the source is producing
set_source_files_properties(
  iso_c_bind.F95
  PROPERTIES
  OBJECT_OUTPUTS ${CMAKE_Fortran_MODULE_DIRECTORY}/iso_c_binding.mod
  )

# State a dependency on the module
set_source_files_properties(
  ieee_arithmetic.F95
  ieee_exceptions.F95
  PROPERTIES
  OBJECT_DEPENDS ${CMAKE_Fortran_MODULE_DIRECTORY}/iso_c_binding.mod
  )

set_target_properties(flang_static flang_shared
  PROPERTIES
  ARCHIVE_OUTPUT_DIRECTORY ${FLANG_RTE_LIB_DIR}
  )
  
target_include_directories(flang_static 
  PRIVATE
  ${CMAKE_CURRENT_SOURCE_DIR}
  ${CMAKE_CURRENT_BINARY_DIR}
  )

target_include_directories(flang_shared
  PRIVATE
  ${CMAKE_CURRENT_SOURCE_DIR}
  ${CMAKE_CURRENT_BINARY_DIR}
  )

# Make sure the compiler is built before we bootstrap
add_dependencies(flang_static 
  flang1
  flang2
  )

# Make sure the compiler is built before we bootstrap
add_dependencies(flang_shared 
  flang1
  flang2
  )

if (NOT MSVC)
  target_compile_options(flang_static PRIVATE -fPIC)
  target_compile_options(flang_shared PRIVATE -fPIC)
else()
  # target_link_libraries(flang_shared Ws2_32.lib)
  # target_link_libraries(flang_static Ws2_32.lib)

  set_target_properties(flang_static PROPERTIES LINKER_LANGUAGE CXX)
  set_target_properties(flang_shared PROPERTIES LINKER_LANGUAGE CXX)
endif()

target_compile_options(flang_static PUBLIC $<$<COMPILE_LANGUAGE:Fortran>:-Mreentrant>)

target_compile_options(flang_shared PUBLIC $<$<COMPILE_LANGUAGE:Fortran>:-Mreentrant>)

