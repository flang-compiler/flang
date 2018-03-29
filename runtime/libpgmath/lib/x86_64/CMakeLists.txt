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

include_directories(${CMAKE_CURRENT_SOURCE_DIR})

add_subdirectory("acos")
add_subdirectory("asin")
add_subdirectory("atan")
add_subdirectory("exp")
add_subdirectory("fast")
add_subdirectory("log")
add_subdirectory("pow")
add_subdirectory("powi")
add_subdirectory("math_tables")
add_subdirectory("relaxed")

get_property(FLAGS GLOBAL PROPERTY "FLAGS_X8664_L1")
get_property(DEFINITIONS GLOBAL PROPERTY "DEFINITIONS_X8664_L1")

set(ASM_SRCS
  aint.S
  anint.S
  around.S
  dint.S
  dnint.S
  dpow.S
  dremainder.S
  dround.S
  dsincos.S
  remainder.S
  sincos.S)

set(SRCS
  abs.c
  acos.c
  acosf.c
  acosh.c
  asin.c
  asinf.c
  asinh.c
  atanh.c
  dacosh.c
  dasinh.c
  datanh.c
  dsqrt.c
  fabs.c
  sqrt.c
  ${ASM_SRCS})
libmath_add_object_library("${SRCS}" "${FLAGS}" "${DEFINITIONS}" "")

# isoc99
set(SRCS
  alog.c
  atan.c
  atan2.c
  cosh.c
  datan.c
  datan2.c
  dcosh.c
  dexp.c
  dexp2.c
  dlog.c
  dsincosp.c
  dsinh.c
  dtan.c
  dtanh.c
  exp.c
  log.c
  rpow.c
  sincosp.c
  sinh.c
  tan.c
  tanh.c)
list(APPEND DEFINITIONS _GNU_SOURCE _ISOC99_SOURCE)
if(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
  list(APPEND DEFINITIONS _GNU_SOURCE)
endif()
set(FLAGS "${FLAGS}-Wall -W -Wstrict-prototypes -Wwrite-strings -Werror ")
libmath_add_object_library("${SRCS}" "${FLAGS}" "${DEFINITIONS}" "isoc99")

# isoc99_log2
set(SRCS
  dlog.c
  alog.c
  log.c)
list(APPEND DEFINITIONS_LOG2 ${DEFINITIONS} COMPILING_LOG2)
libmath_add_object_library("${SRCS}" "${FLAGS}" "${DEFINITIONS_LOG2}" "isoc99_log2")

# isoc99_log10
set(SRCS
  dlog.c
  alog.c
  log.c)
list(APPEND DEFINITIONS_LOG10 ${DEFINITIONS} COMPILING_LOG10)
libmath_add_object_library("${SRCS}" "${FLAGS}" "${DEFINITIONS_LOG10}" "isoc99_log10")
