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
      sdacos.cpp
      ssacos.cpp
      vdacos2.cpp
      vdacos4.cpp
      vsacos4.cpp
      vsacos8.cpp)
    libmath_add_object_library("${SRCS}" "${FLAGS}" "${DEFINITIONS}" "")
    set(SRCS
      vdacos8.cpp
      vsacos16.cpp)
    set(FLAGS_AVX512 "${FLAGS} -march=skylake-avx512 -mtune=skylake-avx512")
    libmath_add_object_library("${SRCS}" "${FLAGS_AVX512}" "${DEFINITIONS}" "acos-avx512")
    set(FLAGS_AVX512 "${FLAGS} -march=knl -mtune=knl")
    libmath_add_object_library("${SRCS}" "${FLAGS_AVX512}" "${DEFINITIONS}" "acos-knl")
  endif()
elseif(${CMAKE_SYSTEM_PROCESSOR} MATCHES "ppc64le" AND NOT ${LIBPGMATH_WITH_GENERIC})
  set(SRCS
    sdacos.cpp
    ssacos.cpp
    vdacos2.cpp
    vsacos4.cpp)
  set_property(SOURCE sdacos.cpp vdacos2.cpp APPEND PROPERTY COMPILE_DEFINITIONS VINT=long)
  set_property(SOURCE ssacos.cpp vsacos4.cpp APPEND PROPERTY COMPILE_DEFINITIONS VINT=)
  libmath_add_object_library("${SRCS}" "${FLAGS}" "${DEFINITIONS}" "")
elseif(${CMAKE_SYSTEM_PROCESSOR} MATCHES "aarch64" AND NOT ${LIBPGMATH_WITH_GENERIC})
  set(SRCS
    sdacos.cpp
    ssacos.cpp
    vdacos2.cpp
    vsacos4.cpp)
  set_property(SOURCE sdacos.cpp vdacos2.cpp APPEND PROPERTY COMPILE_DEFINITIONS VINT=long)
  set_property(SOURCE ssacos.cpp vsacos4.cpp APPEND PROPERTY COMPILE_DEFINITIONS VINT=)
  libmath_add_object_library("${SRCS}" "${FLAGS}" "${DEFINITIONS}" "")
endif()
