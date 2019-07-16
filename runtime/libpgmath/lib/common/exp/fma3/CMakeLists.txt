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
      sdexp.cpp
      ssexp.cpp
      vdexp2.cpp
      vdexp4.cpp
      vsexp4.cpp
      vsexp8.cpp)
    libmath_add_object_library("${SRCS}" "${FLAGS}" "${DEFINITIONS}" "")
    set(SRCS
      vdexp8.cpp
      vsexp16.cpp)
    set(FLAGS_AVX512 "${FLAGS} -march=skylake-avx512 -mtune=skylake-avx512")
    libmath_add_object_library("${SRCS}" "${FLAGS_AVX512}" "${DEFINITIONS}" "exp-avx512")
    set(FLAGS_AVX512 "${FLAGS} -march=knl -mtune=knl")
    libmath_add_object_library("${SRCS}" "${FLAGS_AVX512}" "${DEFINITIONS}" "exp-knl")
  endif()
elseif(${CMAKE_SYSTEM_PROCESSOR} MATCHES "ppc64le" AND NOT ${LIBPGMATH_WITH_GENERIC})
  set(SRCS
    ssexp.cpp)
  set_property(SOURCE ssexp.cpp APPEND PROPERTY COMPILE_DEFINITIONS VINT=)
  libmath_add_object_library("${SRCS}" "${FLAGS}" "${DEFINITIONS}" "")

  set(NEWDEFS ${DEFINITIONS})
  string(REPLACE ";" " -D" NEWDEFS "${NEWDEFS}")
  set(NEWDEFS "-D${NEWDEFS}")

  libmath_add_object_library_asm("sdexp" "${NEWDEFS} -DVINT=long" "${FLAGS}")
  libmath_add_object_library_asm("vdexp2" "${NEWDEFS} -DVINT=long" "${FLAGS}")
  libmath_add_object_library_asm("vsexp4" "${NEWDEFS} -DVINT=" "${FLAGS}")
elseif(${CMAKE_SYSTEM_PROCESSOR} MATCHES "aarch64" AND NOT ${LIBPGMATH_WITH_GENERIC})
  set(SRCS
    ssexp.cpp
    sdexp.cpp
    vdexp2.cpp
    vsexp4.cpp)
  set_property(SOURCE ${SRCS} APPEND_STRING PROPERTY COMPILE_FLAGS "-mcpu=${LLVM_FLANG_CPU_TARGET} ")
  libmath_add_object_library("${SRCS}" "${FLAGS}" "${DEFINITIONS}" "")
endif()
