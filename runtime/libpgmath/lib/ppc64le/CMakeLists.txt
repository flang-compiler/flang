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

add_subdirectory("math_tables")

get_property(FLAGS GLOBAL PROPERTY "FLAGS_PPC64LE_L1")
get_property(DEFINITIONS GLOBAL PROPERTY "DEFINITIONS_PPC64LE_L1")

set(SRCS
  abs.c
  acos.c
  acosh.c
  aint.c
  alog10.c
  alog.c
  anint.c
  around.c
  asin.c
  asinh.c
  atan2.c
  atan.c
  atanh.c
  cos.c
  cosh.c
  dacos.c
  dacosh.c
  dasin.c
  dasinh.c
  datan2.c
  datan.c
  datanh.c
  dcos.c
  dcosh.c
  dexp.c
  dint.c
  dlog10.c
  dlog.c
  dnint.c
  dpowd.c
  dremainder.c
  dround.c
  dsin.c
  dsinh.c
  dsqrt.c
  dtan.c
  dtanh.c
  exp.c
  fabs.c
  idnint.c
  knint.c
  log10.c
  log.c
  mthi64.c
  nint.c
  remainder.c
  round.c
  rpowr.c
  sin.c
  sinh.c
  sqrt.c
  tan.c
  tanh.c
  )
set(FLAGS "${FLAGS} -ffast-math ")
libmath_add_object_library("${SRCS}" "${FLAGS}" "${DEFINITIONS}" "")
