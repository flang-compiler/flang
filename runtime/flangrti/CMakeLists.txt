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

enable_language(C ASM) # Enable assembly

SET(ASM_OPTIONS "-DLINUX_ELF")
SET(CMAKE_ASM_FLAGS "${CFLAGS} ${ASM_OPTIONS}" )

if( ${TARGET_ARCHITECTURE} STREQUAL "x86_64" )
  set(ARCH_DEP_FILES 
    x86_64-Linux/flt_env.c
    x86_64-Linux/x86_daz.c
    x86_64-Linux/x86_flushz.c
    x86_64-Linux/dumpregs.c
  )
elseif( ${TARGET_ARCHITECTURE} STREQUAL "aarch64" )
  set(ARCH_DEP_FILES
      aarch64-Linux/flt_env.c
      aarch64-Linux/dumpregs.c
  )
elseif( ${TARGET_ARCHITECTURE} STREQUAL "ppc64le" )
  set(ARCH_DEP_FILES
    ppc64le-Linux/flt_env.c
    ppc64le-Linux/dumpregs.c
  )
endif()

SET(PGC_SRC_FILES
  ${ARCH_DEP_FILES}
  abs.c
  acos.c
  acosf.c
  acosh.c
  aint.c
  alog10.c
  alog.c
  around.c
  asin.c
  asinf.c
  asinh.c
  atan2.c
  atan.c
  atanh.c
  bessel_j0.c
  bessel_j1.c
  bessel_jn.c
  bessel_tjn.c
  bessel_tyn.c
  bessel_y0.c
  bessel_y1.c
  bessel_yn.c
  cacos.c
  casin.c
  catan.c
  ccosh.c
  cdacos.c
  cdasin.c
  cdatan.c
  cdcosh.c
  cdsinh.c
  cdtan.c
  cdtanh.c
  cos.c
  cosh.c
  csinh.c
  ctan.c
  ctanh.c
  dacosh.c
  dasinh.c
  datan2.c
  datan.c
  datanh.c
  dbessel_j0.c
  dbessel_j1.c
  dbessel_jn.c
  dbessel_tjn.c
  dbessel_tyn.c
  dbessel_y0.c
  dbessel_y1.c
  dbessel_yn.c
  dcos.c
  dcosh.c
  dexp.c
  dint.c
  dlog10.c
  dlog.c
  dpow.c
  dpowi.c
  dpowk.c
  dremainder.c
  dround.c
  dsin.c
  dsinh.c
  dsqrt.c
  dtan.c
  dtanh.c
  erf.c
  erfc.c
  erfcf.c
  erfc_scaled.c
  erfc_scaledf.c
  erff.c
  exp.c
  f2cmain.c
  fabs.c
  gamma.c
  gammaf.c
  hypot.c
  hypotf.c
  ipowi.c
  kidnnt.c
  kpowi.c
  kpowk.c
  ktrap.c
  log10.c
  log.c
  log_gamma.c
  log_gammaf.c
  mpmalloc.c
  mcopy1.c
  mcopy2.c
  mcopy4.c
  mcopy8.c
  mthi64.c
  mset1.c
  mset2.c
  mset4.c
  mset8.c
  mzero1.c
  mzero2.c
  mzero4.c
  mzero8.c
  ioargs.c
  fltmanip.c
  memalign.c
  iostdinit.c
  tempnam.c
  remainder.c
  round.c
  rpow.c
  rpowi.c
  rpowk.c
  s_erf.c
  sin.c
  sinh.c
  sqrt.c
  tan.c
  tanh.c
  trace_lin.c
  trace.c
  anint.c
  dnint.c
  idnint.c
  knint.c
  nint.c
  # src-mp files
  ent3f.h
  hash.c
  hash.h
  llcrit.c
  numa.c
  omp.h
  )

add_flang_library(flangrti_static
  ${PGC_SRC_FILES}
  ${SHARED_SOURCES}
  )

if (MSVC)
  set_property(TARGET flangrti_static PROPERTY OUTPUT_NAME libflangrti)
else()
  set_property(TARGET flangrti_static PROPERTY OUTPUT_NAME flangrti)
endif()

target_include_directories(flangrti_static
  PRIVATE
  ${CMAKE_CURRENT_SOURCE_DIR}
)

set(SHARED_LIBRARY TRUE)
add_flang_library(flangrti_shared
  ${PGC_SRC_FILES}
  ${SHARED_SOURCES}
  )


# Resolve symbols against libm

if (NOT MSVC)
target_link_libraries(flangrti_shared  m)
endif()

# Import OpenMP
if (NOT DEFINED LIBOMP_EXPORT_DIR)
  find_library( 
    FLANG_LIBOMP
    libomp
    HINTS ${CMAKE_BINARY_DIR}/lib)
  target_link_libraries(flangrti_shared PUBLIC ${FLANG_LIBOMP})
endif()

if( ${TARGET_ARCHITECTURE} STREQUAL "aarch64" )
  target_compile_definitions(flangrti_static PRIVATE TARGET_LINUX_ARM)
  target_compile_definitions(flangrti_shared PRIVATE TARGET_LINUX_ARM)
elseif( ${TARGET_ARCHITECTURE} STREQUAL "ppc64le" )
  target_compile_definitions(flangrti_static PRIVATE TARGET_LINUX_POWER)
  target_compile_definitions(flangrti_shared PRIVATE TARGET_LINUX_POWER)
endif()

set_property(TARGET flangrti_shared PROPERTY OUTPUT_NAME flangrti)
set(SHARED_LIBRARY FALSE)

target_include_directories(flangrti_static
  PRIVATE
  ${CMAKE_CURRENT_SOURCE_DIR}
  )
  
target_include_directories(flangrti_shared
  PRIVATE
  ${CMAKE_CURRENT_SOURCE_DIR}
)

set_target_properties(flangrti_shared flangrti_static
                      PROPERTIES ARCHIVE_OUTPUT_DIRECTORY ${FLANG_RTE_LIB_DIR})

if (NOT MSVC)
  target_compile_options(flangrti_static PRIVATE -fPIC)
  target_compile_options(flangrti_shared PRIVATE -fPIC)
else()
  set_target_properties(flangrti_shared PROPERTIES WINDOWS_EXPORT_ALL_SYMBOLS TRUE)
endif()

target_compile_options(flangrti_static PUBLIC $<$<COMPILE_LANGUAGE:Fortran>:-Mreentrant>)

target_compile_options(flangrti_shared PUBLIC $<$<COMPILE_LANGUAGE:Fortran>:-Mreentrant>)

