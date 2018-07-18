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

# Set source files and compilation flags
set(COREAVX2_SRCS fs_sin_1_avx2.cpp fs_sin_4_avx2.cpp fs_sin_8_avx2.cpp)
set_property(SOURCE ${COREAVX2_SRCS} APPEND_STRING PROPERTY COMPILE_FLAGS "-mtune=core-avx2 -march=core-avx2 ")
set(SKYLAKE_SRCS fs_sin_16_avx512.cpp)
set_property(SOURCE ${SKYLAKE_SRCS} APPEND_STRING PROPERTY COMPILE_FLAGS "-mtune=skylake-avx512 -march=skylake-avx512 ")

# Set source files
set(SRCS
  ${COREAVX2_SRCS}
  ${SKYLAKE_SRCS})
set_property(SOURCE ${SRCS} APPEND_STRING PROPERTY COMPILE_FLAGS "-Wno-attributes ")
set_property(SOURCE ${SRCS} APPEND_STRING PROPERTY COMPILE_DEFINITIONS "PGI ")

libmath_add_object_library("${SRCS}" "${FLAGS}" "${DEFINITIONS}" "")
