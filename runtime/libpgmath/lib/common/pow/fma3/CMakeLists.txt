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
      sdpow.cpp
      sspow.cpp
      vdpow2.cpp
      vdpow4.cpp
      vspow4.cpp
      vspow8.cpp)
    libmath_add_object_library("${SRCS}" "${FLAGS}" "${DEFINITIONS}" "")
    set(SRCS
      vdpow8.cpp
      vspow16.cpp)
    set(FLAGS_AVX512 "${FLAGS} -march=skylake-avx512 -mtune=skylake-avx512")
    libmath_add_object_library("${SRCS}" "${FLAGS_AVX512}" "${DEFINITIONS}" "pow-avx512")
    set(FLAGS_AVX512 "${FLAGS} -march=knl -mtune=knl")
    libmath_add_object_library("${SRCS}" "${FLAGS_AVX512}" "${DEFINITIONS}" "pow-knl")
  endif()
elseif(${CMAKE_SYSTEM_PROCESSOR} MATCHES "ppc64le" AND NOT ${LIBPGMATH_WITH_GENERIC})
  set(SRCS
    sdpow.cpp
    sspow.cpp
    vdpow2.cpp
    vspow4.cpp)
  set_property(SOURCE sdpow.cpp vdpow2.cpp APPEND PROPERTY COMPILE_DEFINITIONS VINT=long)
  set_property(SOURCE sspow.cpp vspow4.cpp APPEND PROPERTY COMPILE_DEFINITIONS VINT=)
  libmath_add_object_library("${SRCS}" "${FLAGS}" "${DEFINITIONS}" "")
elseif(${CMAKE_SYSTEM_PROCESSOR} MATCHES "aarch64" AND NOT ${LIBPGMATH_WITH_GENERIC})
  set(SRCS
    sdpow.cpp
    sspow.cpp
    vdpow2.cpp)
  set_property(SOURCE sdpow.cpp vdpow2.cpp APPEND PROPERTY COMPILE_DEFINITIONS VINT=long)
  set_property(SOURCE sspow.cpp vspow4.cpp APPEND PROPERTY COMPILE_DEFINITIONS VINT=)
  set_property(SOURCE ${SRCS} APPEND_STRING PROPERTY COMPILE_FLAGS "-mcpu=${LLVM_FLANG_CPU_TARGET} ")
endif()
