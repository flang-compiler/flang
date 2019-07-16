#
# Copyright (c) 2018, NVIDIA CORPORATION.  All rights reserved.
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

# Set compiler definitions and flags
get_property(DEFINITIONS GLOBAL PROPERTY "DEFINITIONS_L2")
get_property(FLAGS GLOBAL PROPERTY "FLAGS_L2")

set(SRCS)

# Set source files, compiler flags and definitions
if(${CMAKE_SYSTEM_PROCESSOR} MATCHES "x86_64" AND NOT ${LIBPGMATH_WITH_GENERIC})
  if(${CMAKE_SYSTEM_NAME} MATCHES "Linux|Darwin")
    set(SRCS
      fdlog1.cpp
      fdlog2.cpp
      fdlog4.cpp
      fslog1.cpp
      fslog4.cpp
      fslog8.cpp)
    # Additional files with custom compilation flags
    set(SRCS_ADD
      rslog1.cpp
      rslog4.cpp
      rslog8.cpp)
    set_property(SOURCE ${SRCS_ADD} APPEND PROPERTY COMPILE_DEFINITIONS PRECISION=2)
    set(SRCS
      ${SRCS}
      ${SRCS_ADD})
    libmath_add_object_library("${SRCS}" "${FLAGS}" "${DEFINITIONS}" "")
    set(SRCS
      fdlog8.cpp
      fslog16.cpp
      rslog16.cpp)
    set(FLAGS_AVX512 "${FLAGS} -march=skylake-avx512 -mtune=skylake-avx512")
    libmath_add_object_library("${SRCS}" "${FLAGS_AVX512}" "${DEFINITIONS}" "log-avx512")
    set(FLAGS_AVX512 "${FLAGS} -march=knl -mtune=knl")
    libmath_add_object_library("${SRCS}" "${FLAGS_AVX512}" "${DEFINITIONS}" "log-knl")
  endif()
elseif(${CMAKE_SYSTEM_PROCESSOR} MATCHES "ppc64le" AND NOT ${LIBPGMATH_WITH_GENERIC})
  set(INT_SRCS
    fslog1.cpp
    fslog4.cpp
    rslog1.cpp
    rslog4.cpp)
  set(LONG_SRCS
    fdlog1.cpp
    fdlog2.cpp)
  set(SRCS
    ${INT_SRCS}
    ${LONG_SRCS})
  set_property(SOURCE ${INT_SRCS} APPEND PROPERTY COMPILE_DEFINITIONS VINT=)
  set_property(SOURCE ${LONG_SRCS} APPEND PROPERTY COMPILE_DEFINITIONS VINT=long)
  libmath_add_object_library("${SRCS}" "${FLAGS}" "${DEFINITIONS}" "")
elseif(${CMAKE_SYSTEM_PROCESSOR} MATCHES "aarch64" AND NOT ${LIBPGMATH_WITH_GENERIC})
  set(INT_SRCS
    fslog1.cpp
    fslog4.cpp
    rslog1.cpp
    rslog4.cpp)
  set(LONG_SRCS
    fdlog1.cpp
    fdlog2.cpp)
  set(SRCS
    ${INT_SRCS}
    ${LONG_SRCS})
  set_property(SOURCE ${INT_SRCS} APPEND PROPERTY COMPILE_DEFINITIONS VINT=)
  set_property(SOURCE ${LONG_SRCS} APPEND PROPERTY COMPILE_DEFINITIONS VINT=long)
  set_property(SOURCE ${SRCS} APPEND_STRING PROPERTY COMPILE_FLAGS "-mcpu=${LLVM_FLANG_CPU_TARGET} ")
endif()

