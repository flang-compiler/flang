#
# Copyright (c) 2018-2019, NVIDIA CORPORATION.  All rights reserved.
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

add_subdirectory("fma3")

# Set compiler flags and definitions
get_property(DEFINITIONS GLOBAL PROPERTY "DEFINITIONS_L1")
get_property(FLAGS GLOBAL PROPERTY "FLAGS_L1")

set(SRCS)

if(${LIBPGMATH_SYSTEM_PROCESSOR} MATCHES "aarch64")
  set(SRCS gslog4.c gdlog2.c)
  libmath_add_object_library("${SRCS}" "${FLAGS}" "${DEFINITIONS}" "")
endif()


set(SRCS_SCALAR
  fd_log_scalar.cpp
  )

set(SRCS_VECTOR
  fd_log_vector.cpp
  )


if(${LIBPGMATH_SYSTEM_PROCESSOR} MATCHES "x86_64")

# Scalar 
set(FLAGS_TMP "${FLAGS} -mtune=core-avx2 -march=core-avx2 -D_CPU=avx2")
libmath_add_object_library("${SRCS_SCALAR}" "${FLAGS_TMP}" "${DEFINITIONS}" "log-avx2_1")

set(FLAGS_TMP "${FLAGS} -mtune=skylake-avx512 -march=skylake-avx512 -D_CPU=avx512")
libmath_add_object_library("${SRCS_SCALAR}" "${FLAGS_TMP}" "${DEFINITIONS}" "log-avx512_1")


# Vector, 2 elements
set(FLAGS_TMP "${FLAGS} -mtune=core-avx2 -march=core-avx2 -D_CPU=avx2 -D_VL=2")
libmath_add_object_library("${SRCS_VECTOR}" "${FLAGS_TMP}" "${DEFINITIONS}" "log-avx2_2")

set(FLAGS_TMP "${FLAGS} -mtune=skylake-avx512 -march=skylake-avx512 -D_CPU=avx512 -D_VL=2")
libmath_add_object_library("${SRCS_VECTOR}" "${FLAGS_TMP}" "${DEFINITIONS}" "log-avx512_2")


# Vector, 4 elements
set(FLAGS_TMP "${FLAGS} -mtune=core-avx2 -march=core-avx2 -D_CPU=avx2 -D_VL=4")
libmath_add_object_library("${SRCS_VECTOR}" "${FLAGS_TMP}" "${DEFINITIONS}" "log-avx2_4")

set(FLAGS_TMP "${FLAGS} -mtune=skylake-avx512 -march=skylake-avx512 -D_CPU=avx512 -D_VL=4")
libmath_add_object_library("${SRCS_VECTOR}" "${FLAGS_TMP}" "${DEFINITIONS}" "log-avx512_4")


# Vector, 8 elements
set(FLAGS_TMP "${FLAGS} -mtune=knl -march=knl -D_CPU=knl -D_VL=8")
libmath_add_object_library("${SRCS_VECTOR}" "${FLAGS_TMP}" "${DEFINITIONS}" "log-knl_8")

set(FLAGS_TMP "${FLAGS} -mtune=skylake-avx512 -march=skylake-avx512 -D_CPU=avx512 -D_VL=8")
libmath_add_object_library("${SRCS_VECTOR}" "${FLAGS_TMP}" "${DEFINITIONS}" "log-avx512_8")

endif()

