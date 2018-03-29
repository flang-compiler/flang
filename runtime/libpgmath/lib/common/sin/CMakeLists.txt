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

# Set compiler flags and definitions
get_property(DEFINITIONS GLOBAL PROPERTY "DEFINITIONS_L1")
get_property(FLAGS GLOBAL PROPERTY "FLAGS_L1")

set(SRCS)

# Set source files, compiler flags and definitions
if(${CMAKE_SYSTEM_PROCESSOR} MATCHES "x86_64")
  if(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
    set(CORE2_SRCS gdsin2.c gssin4.c)
    set_property(SOURCE ${CORE2_SRCS} APPEND_STRING PROPERTY COMPILE_FLAGS "-march=core2 ")
    set(COREAVX2_SRCS fd_sin_1_avx2.cpp fd_sin_2_avx2.cpp fd_sin_4_avx2.cpp)
    set_property(SOURCE ${COREAVX2_SRCS} APPEND_STRING PROPERTY COMPILE_FLAGS "-Wno-attributes -mtune=core-avx2 -march=core-avx2 ")
    set_property(SOURCE ${COREAVX2_SRCS} APPEND_STRING PROPERTY COMPILE_DEFINITIONS "PGI ")
    set(SANDYBRIDGE_SRCS gdsin4.c gssin8.c)
    set_property(SOURCE ${SANDYBRIDGE_SRCS} APPEND_STRING PROPERTY COMPILE_FLAGS "-march=sandybridge ")
    set(KNL_SRCS gdsin8.c gssin16.c)
    set_property(SOURCE ${KNL_SRCS} APPEND_STRING PROPERTY COMPILE_FLAGS "-mtune=knl -march=knl ")
    set(SKYLAKE_SRCS fd_sin_8_avx512.cpp)
    set_property(SOURCE ${SKYLAKE_SRCS} APPEND_STRING PROPERTY COMPILE_FLAGS "-Wno-attributes -mtune=skylake-avx512 -march=skylake-avx512 ")
    set_property(SOURCE ${SKYLAKE_SRCS} APPEND_STRING PROPERTY COMPILE_DEFINITIONS "PGI ")
    set(SRCS
      ${CORE2_SRCS}
      ${COREAVX2_SRCS}
      ${SANDYBRIDGE_SRCS}
      ${KNL_SRCS}
      ${SKYLAKE_SRCS})
  elseif(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
    set(CORE2_SRCS gdsin2.c gssin4.c)
    set_property(SOURCE ${CORE2_SRCS} APPEND_STRING PROPERTY COMPILE_FLAGS "-msse2 ")
    set(COREAVX2_SRCS fd_sin_1_avx2.cpp fd_sin_2_avx2.cpp fd_sin_4_avx2.cpp)
    set_property(SOURCE ${COREAVX2_SRCS} APPEND_STRING PROPERTY COMPILE_FLAGS "-Wno-attributes -mavx2 -mfma ")
    set_property(SOURCE ${COREAVX2_SRCS} APPEND_STRING PROPERTY COMPILE_DEFINITIONS "PGI ")
    set(SANDYBRIDGE_SRCS gdsin4.c gssin8.c)
    set_property(SOURCE ${SANDYBRIDGE_SRCS} APPEND_STRING PROPERTY COMPILE_FLAGS "-mavx ")
    set(KNL_SRCS gdsin8.c gssin16.c)
    set_property(SOURCE ${KNL_SRCS} APPEND_STRING PROPERTY COMPILE_FLAGS "-mavx512f ")
    set(SKYLAKE_SRCS fd_sin_8_avx512.cpp)
    set_property(SOURCE ${SKYLAKE_SRCS} APPEND_STRING PROPERTY COMPILE_FLAGS "-Wno-attributes -mtune=skylake-avx512 -march=skylake-avx512 ")
    set_property(SOURCE ${SKYLAKE_SRCS} APPEND_STRING PROPERTY COMPILE_DEFINITIONS "PGI ")
    set(SRCS
      ${CORE2_SRCS}
      ${COREAVX2_SRCS}
      ${SANDYBRIDGE_SRCS}
      ${KNL_SRCS}
      ${SKYLAKE_SRCS})
  endif()
elseif(${CMAKE_SYSTEM_PROCESSOR} MATCHES "ppc64le")
  set(SRCS gssin4.c gdsin2.c)
  set_property(SOURCE ${SRCS} APPEND_STRING PROPERTY COMPILE_FLAGS "-mcpu=power8 ")
endif()

libmath_add_object_library("${SRCS}" "${FLAGS}" "${DEFINITIONS}" "")
