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

set(SRCS gsincos.c)

# Set source files, compiler flags and definitions
if(${CMAKE_SYSTEM_PROCESSOR} MATCHES "x86_64")
  if(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
    libmath_add_object_library("${SRCS}" "${FLAGS}" "${DEFINITIONS}" "gssincos4")
    set_property(TARGET gssincos4 APPEND PROPERTY COMPILE_DEFINITIONS SINCOS=__mth_sincos PRECSIZE=4 VLEN=4)
    libmath_add_object_library("${SRCS}" "${FLAGS}" "${DEFINITIONS}" "gdsincos2")
    set_property(TARGET gdsincos2 APPEND PROPERTY COMPILE_DEFINITIONS SINCOS=__mth_dsincos PRECSIZE=8 VLEN=2)
    set_property(TARGET gssincos4 gdsincos2 APPEND_STRING PROPERTY COMPILE_FLAGS "-march=core2 ")

    libmath_add_object_library("${SRCS}" "${FLAGS}" "${DEFINITIONS}" "gssincos8")
    set_property(TARGET gssincos8 APPEND PROPERTY COMPILE_DEFINITIONS SINCOS=__mth_sincos PRECSIZE=4 VLEN=8)
    libmath_add_object_library("${SRCS}" "${FLAGS}" "${DEFINITIONS}" "gdsincos4")
    set_property(TARGET gdsincos4 APPEND PROPERTY COMPILE_DEFINITIONS SINCOS=__mth_dsincos PRECSIZE=8 VLEN=4)
    set_property(TARGET gssincos8 gdsincos4 APPEND_STRING PROPERTY COMPILE_FLAGS "-march=sandybridge ")

    libmath_add_object_library("${SRCS}" "${FLAGS}" "${DEFINITIONS}" "gssincos16")
    set_property(TARGET gssincos16 APPEND PROPERTY COMPILE_DEFINITIONS SINCOS=__mth_sincos PRECSIZE=4 VLEN=16)
    libmath_add_object_library("${SRCS}" "${FLAGS}" "${DEFINITIONS}" "gdsincos8")
    set_property(TARGET gdsincos8 APPEND PROPERTY COMPILE_DEFINITIONS SINCOS=__mth_dsincos PRECSIZE=8 VLEN=8)
    set_property(TARGET gssincos16 gdsincos8 APPEND_STRING PROPERTY COMPILE_FLAGS "-mtune=knl -march=knl ")

    # Set source files and compilation flags
    set(COREAVX2_SRCS fd_sincos_1_avx2.c fd_sincos_2_avx2.c fd_sincos_4_avx2.c)
    set_property(SOURCE ${COREAVX2_SRCS} APPEND_STRING PROPERTY COMPILE_FLAGS "-Wno-attributes -mavx2 -mfma ")
    set_property(SOURCE ${COREAVX2_SRCS} APPEND PROPERTY COMPILE_DEFINITIONS "PGI ")
    set(SKYLAKE_SRCS fd_sincos_8_avx512.c)
    set_property(SOURCE ${SKYLAKE_SRCS} APPEND PROPERTY COMPILE_FLAGS "-Wno-attributes -mtune=skylake-avx512 -march=skylake-avx512 ")
    set_property(SOURCE ${SKYLAKE_SRCS} APPEND PROPERTY COMPILE_DEFINITIONS "PGI ")

    # Set source files
    set(SRCS
      ${COREAVX2_SRCS}
      ${SKYLAKE_SRCS})
    libmath_add_object_library("${SRCS}" "${FLAGS}" "${DEFINITIONS}" "")
  elseif(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
    libmath_add_object_library("${SRCS}" "${FLAGS}" "${DEFINITIONS}" "gssincos4")
    set_property(TARGET gssincos4 APPEND PROPERTY COMPILE_DEFINITIONS SINCOS=__mth_sincos PRECSIZE=4 VLEN=4)
    libmath_add_object_library("${SRCS}" "${FLAGS}" "${DEFINITIONS}" "gdsincos2")
    set_property(TARGET gdsincos2 APPEND PROPERTY COMPILE_DEFINITIONS SINCOS=__mth_dsincos PRECSIZE=8 VLEN=2)
    set_property(TARGET gssincos4 gdsincos2 APPEND_STRING PROPERTY COMPILE_FLAGS "-msse2 ")

    libmath_add_object_library("${SRCS}" "${FLAGS}" "${DEFINITIONS}" "gssincos8")
    set_property(TARGET gssincos8 APPEND PROPERTY COMPILE_DEFINITIONS SINCOS=__mth_sincos PRECSIZE=4 VLEN=8)
    libmath_add_object_library("${SRCS}" "${FLAGS}" "${DEFINITIONS}" "gdsincos4")
    set_property(TARGET gdsincos4 APPEND PROPERTY COMPILE_DEFINITIONS SINCOS=__mth_dsincos PRECSIZE=8 VLEN=4)
    set_property(TARGET gssincos8 gdsincos4 APPEND_STRING PROPERTY COMPILE_FLAGS "-mavx ")

    libmath_add_object_library("${SRCS}" "${FLAGS}" "${DEFINITIONS}" "gssincos16")
    set_property(TARGET gssincos16 APPEND PROPERTY COMPILE_DEFINITIONS SINCOS=__mth_sincos PRECSIZE=4 VLEN=16)
    libmath_add_object_library("${SRCS}" "${FLAGS}" "${DEFINITIONS}" "gdsincos8")
    set_property(TARGET gdsincos8 APPEND PROPERTY COMPILE_DEFINITIONS SINCOS=__mth_dsincos PRECSIZE=8 VLEN=8)
    set_property(TARGET gssincos16 gdsincos8 APPEND_STRING PROPERTY COMPILE_FLAGS "-mavx512f -mfma ")

    # Set source files and compilation flags
    set(COREAVX2_SRCS fd_sincos_1_avx2.c fd_sincos_2_avx2.c fd_sincos_4_avx2.c)
    set_property(SOURCE ${COREAVX2_SRCS} APPEND_STRING PROPERTY COMPILE_FLAGS "-Wno-attributes -mavx2 -mfma ")
    set_property(SOURCE ${COREAVX2_SRCS} APPEND PROPERTY COMPILE_DEFINITIONS "PGI ")
    set(SKYLAKE_SRCS fd_sincos_8_avx512.c)
    set_property(SOURCE ${SKYLAKE_SRCS} APPEND PROPERTY COMPILE_FLAGS "-Wno-attributes -mtune=skylake-avx512 -march=skylake-avx512 ")
    set_property(SOURCE ${SKYLAKE_SRCS} APPEND PROPERTY COMPILE_DEFINITIONS "PGI ")

    # Set source files
    set(SRCS
      ${COREAVX2_SRCS}
      ${SKYLAKE_SRCS})
    libmath_add_object_library("${SRCS}" "${FLAGS}" "${DEFINITIONS}" "")
  endif()
elseif(${CMAKE_SYSTEM_PROCESSOR} MATCHES "ppc64le")
  set_property(SOURCE ${SRCS} APPEND_STRING PROPERTY COMPILE_FLAGS "-mcpu=power8 ")

  libmath_add_object_library("${SRCS}" "${FLAGS}" "${DEFINITIONS}" "gssincos4")
  set_property(TARGET gssincos4 APPEND PROPERTY COMPILE_DEFINITIONS SINCOS=sincos PRECSIZE=8 VLEN=2)

  libmath_add_object_library("${SRCS}" "${FLAGS}" "${DEFINITIONS}" "gdsincos2")
  set_property(TARGET gdsincos2 APPEND PROPERTY COMPILE_DEFINITIONS SINCOS=sincosf PRECSIZE=4 VLEN=4)
endif()
