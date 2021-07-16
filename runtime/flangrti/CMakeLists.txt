
#
# Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
# See https://llvm.org/LICENSE.txt for license information.
# SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
#

enable_language(C ASM) # Enable assembly

SET(ASM_OPTIONS "-DLINUX_ELF")
SET(CMAKE_ASM_FLAGS "${CFLAGS} ${ASM_OPTIONS}" )

if( ${TARGET_ARCHITECTURE} STREQUAL "x86_64" )
  set(ARCH_DEP_FILES
    x86_64-Linux/x86_daz.c
    x86_64-Linux/x86_flushz.c
    x86_64-Linux/dumpregs.c
  )
elseif( ${TARGET_ARCHITECTURE} STREQUAL "aarch64" )
  set(ARCH_DEP_FILES
      aarch64-Linux/dumpregs.c
  )
elseif( ${TARGET_ARCHITECTURE} STREQUAL "ppc64le" )
  set(ARCH_DEP_FILES
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
if (MSVC)
  set_property(TARGET flangrti_static PROPERTY OUTPUT_NAME libflangrti)
else()
  set_property(TARGET flangrti_static PROPERTY OUTPUT_NAME flangrti)
endif()


set(SHARED_LIBRARY TRUE)
add_flang_library(flangrti_shared
  ${PGC_SRC_FILES}
  ${SHARED_SOURCES}
  )

# Resolve symbols against libm
if (NOT MSVC)
  target_link_libraries(flangrti_shared PRIVATE m)
endif()


# Resolve symbols against libpthread
find_package(Threads REQUIRED)
if (CMAKE_THREAD_LIBS_INIT)
  target_link_libraries(flangrti_shared PRIVATE "${CMAKE_THREAD_LIBS_INIT}")
endif()

# Import OpenMP
if (NOT DEFINED LIBOMP_EXPORT_DIR)
  find_library(
    FLANG_LIBOMP
    NAMES omp libomp
    HINTS ${CMAKE_BINARY_DIR}/lib)
  target_link_libraries(flangrti_shared PRIVATE ${FLANG_LIBOMP})
endif()

find_library(
  LIBPGMATH
  NAMES pgmath libpgmath
  HINTS ${CMAKE_BINARY_DIR}/lib)
target_link_libraries(flangrti_shared PRIVATE ${LIBPGMATH})

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

