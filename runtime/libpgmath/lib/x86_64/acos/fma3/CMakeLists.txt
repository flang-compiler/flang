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
get_property(DEFINITIONS GLOBAL PROPERTY "DEFINITIONS_X8664_L2")
get_property(FLAGS GLOBAL PROPERTY "FLAGS_X8664_L2")

# Set source files
set(SRCS
  vdacos_mask.S
  vsacos_mask.S)
libmath_add_object_library("${SRCS}" "${FLAGS}" "${DEFINITIONS}" "")

if(${CMAKE_SYSTEM_NAME} MATCHES "Linux|Darwin")
  set(SRCS
    vdacos8.cpp
    vsacos16.cpp)
  set(FLAGS_AVX512 "${FLAGS} -march=skylake-avx512 -mtune=skylake-avx512")
  string(REPLACE "x86_64" "common" INCLUDE_DIR ${CMAKE_CURRENT_SOURCE_DIR})
  libmath_add_object_library("${SRCS}" "${FLAGS_AVX512}" "${DEFINITIONS}" "acos-avx512")
  target_include_directories(acos-avx512 
    PRIVATE ${INCLUDE_DIR})
  set(FLAGS_AVX512 "${FLAGS} -march=knl -mtune=knl")
  libmath_add_object_library("${SRCS}" "${FLAGS_AVX512}" "${DEFINITIONS}" "acos-knl")
  target_include_directories(acos-knl 
    PRIVATE ${INCLUDE_DIR})
endif()
