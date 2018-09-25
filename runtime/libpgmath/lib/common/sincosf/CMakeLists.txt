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

set(FLAGS "${FLAGS} -Wno-attributes")
set(DEFINITIONS "${DEFINITIONS} -DPGI")
set(SRC_VECTOR fs_sincos_vector.cpp)
set(SRC_SCALAR fs_sincos_scalar.cpp)

set(AVX2_FLAGS "${FLAGS} -mtune=core-avx2 -march=core-avx2")
set(AVX512_FLAGS "${FLAGS} -mtune=skylake-avx512 -march=skylake-avx512")
set(KNL_FLAGS "${FLAGS} -mtune=knl -march=knl")

# SINCOS
libmath_add_object_library("${SRC_SCALAR}" "${AVX2_FLAGS}" "${DEFINITIONS} -DSINCOS" "fs_sincos_1_avx2")
libmath_add_object_library("${SRC_VECTOR}" "${AVX2_FLAGS}" "${DEFINITIONS} -DSINCOS -DVL=4" "fs_sincos_4_avx2")
libmath_add_object_library("${SRC_VECTOR}" "${AVX2_FLAGS}" "${DEFINITIONS} -DSINCOS -DVL=8" "fs_sincos_8_avx2")
libmath_add_object_library("${SRC_VECTOR}" "${AVX512_FLAGS}" "${DEFINITIONS} -DSINCOS -DVL=16" "fs_sincos_16_avx2")
libmath_add_object_library("${SRC_VECTOR}" "${KNL_FLAGS}" "${DEFINITIONS} -DSINCOS -DVL=16" "fs_sincos_16_knl")

# COSINE
libmath_add_object_library("${SRC_SCALAR}" "${AVX2_FLAGS}" "${DEFINITIONS} -DCOSINE" "fs_cos_1_avx2")
libmath_add_object_library("${SRC_VECTOR}" "${AVX2_FLAGS}" "${DEFINITIONS} -DCOSINE -DVL=4" "fs_cos_4_avx2")
libmath_add_object_library("${SRC_VECTOR}" "${AVX2_FLAGS}" "${DEFINITIONS} -DCOSINE -DVL=8" "fs_cos_8_avx2")
libmath_add_object_library("${SRC_VECTOR}" "${AVX512_FLAGS}" "${DEFINITIONS} -DCOSINE -DVL=16" "fs_cos_16_avx2")
libmath_add_object_library("${SRC_VECTOR}" "${KNL_FLAGS}" "${DEFINITIONS} -DCOSINE -DVL=16" "fs_cos_16_knl")

# SINE
libmath_add_object_library("${SRC_SCALAR}" "${AVX2_FLAGS}" "${DEFINITIONS} -DSINE" "fs_sin_1_avx2")
libmath_add_object_library("${SRC_VECTOR}" "${AVX2_FLAGS}" "${DEFINITIONS} -DSINE -DVL=4" "fs_sin_4_avx2")
libmath_add_object_library("${SRC_VECTOR}" "${AVX2_FLAGS}" "${DEFINITIONS} -DSINE -DVL=8" "fs_sin_8_avx2")
libmath_add_object_library("${SRC_VECTOR}" "${AVX512_FLAGS}" "${DEFINITIONS} -DSINE -DVL=16" "fs_sin_16_avx2")
libmath_add_object_library("${SRC_VECTOR}" "${KNL_FLAGS}" "${DEFINITIONS} -DSINE -DVL=16" "fs_sin_16_knl")
