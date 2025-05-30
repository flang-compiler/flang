#
# Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
# See https://llvm.org/LICENSE.txt for license information.
# SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
#

# Set compiler definitions and flags
get_property(DEFINITIONS GLOBAL PROPERTY "DEFINITIONS_L2")
get_property(FLAGS GLOBAL PROPERTY "FLAGS_L2")

set(SRCS)

# Set source files, compiler flags and definitions
if(${LIBPGMATH_SYSTEM_PROCESSOR} MATCHES "x86_64" AND NOT ${LIBPGMATH_WITH_GENERIC})
  if(${LIBPGMATH_SYSTEM_NAME} MATCHES "Linux|Darwin|Windows")
    set(SRCS
      sdasin.cpp
      ssasin.cpp
      vdasin2.cpp
      vdasin4.cpp
      vsasin4.cpp
      vsasin8.cpp)
    libmath_add_object_library("${SRCS}" "${FLAGS} -mfma -mavx2" "${DEFINITIONS}" "")
    set(SRCS
      vdasin8.cpp
      vsasin16.cpp)
    set(FLAGS_AVX512 "${FLAGS} -march=skylake-avx512 -mtune=skylake-avx512")
    libmath_add_object_library("${SRCS}" "${FLAGS_AVX512}" "${DEFINITIONS}" "asin-avx512")
    set(FLAGS_AVX512 "${FLAGS} -march=knl -mtune=knl")
    libmath_add_object_library("${SRCS}" "${FLAGS_AVX512}" "${DEFINITIONS}" "asin-knl")
  endif()
elseif(${LIBPGMATH_SYSTEM_PROCESSOR} MATCHES "ppc64le" AND NOT ${LIBPGMATH_WITH_GENERIC})
  set(SRCS
    sdasin.cpp
    ssasin.cpp
    vdasin2.cpp
    vsasin4.cpp)
  set_property(SOURCE sdasin.cpp vdasin2.cpp APPEND PROPERTY COMPILE_DEFINITIONS VINT=long)
  set_property(SOURCE ssasin.cpp vsasin4.cpp APPEND PROPERTY COMPILE_DEFINITIONS VINT=)
  libmath_add_object_library("${SRCS}" "${FLAGS}" "${DEFINITIONS}" "")
elseif(${LIBPGMATH_SYSTEM_PROCESSOR} MATCHES "aarch64" AND NOT ${LIBPGMATH_WITH_GENERIC})
  set(SRCS
    sdasin.cpp
    ssasin.cpp
    vdasin2.cpp
    vsasin4.cpp)
  set_property(SOURCE sdasin.cpp vdasin2.cpp APPEND PROPERTY COMPILE_DEFINITIONS VINT=long)
  set_property(SOURCE ssasin.cpp vsasin4.cpp APPEND PROPERTY COMPILE_DEFINITIONS VINT=)
  libmath_add_object_library("${SRCS}" "${FLAGS}" "${DEFINITIONS}" "")
endif()

