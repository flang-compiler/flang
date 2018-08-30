#
# Copyright (c) 2017-2018, NVIDIA CORPORATION.  All rights reserved.
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
  bessel_tjn.c
  bessel_tyn.c
  dbessel_tjn.c
  dbessel_tyn.c
  f2cmain.c
  kidnnt.c
  ktrap.c
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
  round.c
  s_erf.c
  trace_lin.c
  trace.c
  idnint.c
  knint.c
  mpmalloc.c
  nint.c
  # src-mp files
  llcrit.c
  numa.c
  )

add_flang_library(flangrti_static
  ${PGC_SRC_FILES}
  ${SHARED_SOURCES}
  )
set_property(TARGET flangrti_static PROPERTY OUTPUT_NAME flangrti)


set(SHARED_LIBRARY TRUE)
add_flang_library(flangrti_shared
  ${PGC_SRC_FILES}
  ${SHARED_SOURCES}
  )

# Resolve symbols against libm
target_link_libraries(flangrti_shared m)

# Import OpenMP
if (NOT DEFINED LIBOMP_EXPORT_DIR)
  find_library( 
    FLANG_LIBOMP
    libomp.so
    HINTS ${CMAKE_BINARY_DIR}/lib)
  target_link_libraries(flangrti_shared ${FLANG_LIBOMP})
endif()

find_library( 
  LIBPGMATH
  libpgmath.so
  HINTS ${CMAKE_BINARY_DIR}/lib)
target_link_libraries(flangrti_shared ${LIBPGMATH})

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

target_compile_options(flangrti_static PRIVATE -fPIC)

target_compile_options(flangrti_shared PRIVATE -fPIC)

target_compile_options(flangrti_static PUBLIC $<$<COMPILE_LANGUAGE:Fortran>:-Mreentrant>)

target_compile_options(flangrti_shared PUBLIC $<$<COMPILE_LANGUAGE:Fortran>:-Mreentrant>)

