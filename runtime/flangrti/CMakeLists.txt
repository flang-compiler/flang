#
# Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
# See https://llvm.org/LICENSE.txt for license information.
# SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
#

enable_language(C ASM) # Enable assembly

SET(ASM_OPTIONS "-DLINUX_ELF")
SET(CMAKE_ASM_FLAGS "${CFLAGS} ${ASM_OPTIONS}" )

set(ARCH_DEP_FILES
  ${TARGET_ARCHITECTURE}/dumpregs.c
)

if( ${TARGET_ARCHITECTURE} STREQUAL "x86_64" )
  set(ARCH_DEP_FILES ${ARCH_DEP_FILES}
    x86_64/x86_daz.c
    x86_64/x86_flushz.c
  )
endif()

SET(PGC_SRC_FILES
  ${ARCH_DEP_FILES}
  bessel_tjn.c
  bessel_tyn.c
  dbessel_tjn.c
  dbessel_tyn.c
  f2cmain.c
  ktrap.c
  mcopy1.c
  mcopy2.c
  mcopy4.c
  mcopy8.c
  mset1.c
  mset2.c
  mset4.c
  mset8.c
  mzero1.c
  mzero2.c
  mzero4.c
  mzero8.c
  ioargs.c
  memalign.c
  iostdinit.c
  tempnam.c
  s_erf.c
  trace_lin.c
  trace.c
  mpmalloc.c
  # src-mp files
  llcrit.c
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

# Resolve symbols against libpthread
find_package(Threads REQUIRED)
if (CMAKE_THREAD_LIBS_INIT)
  target_link_libraries(flangrti_shared "${CMAKE_THREAD_LIBS_INIT}")
endif()

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

