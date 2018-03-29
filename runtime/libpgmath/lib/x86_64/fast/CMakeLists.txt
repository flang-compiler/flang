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
get_property(DEFINITIONS GLOBAL PROPERTY "DEFINITIONS_X8664_L2")
get_property(FLAGS GLOBAL PROPERTY "FLAGS_X8664_L2")

# Set source files
set(SRCS
  fastcdiv.S
  fastmod.S)
libmath_add_object_library("${SRCS}" "${FLAGS}" "${DEFINITIONS}" "")

set(TARGET_NAME "mthdecls")
list(APPEND PREPROCESSOR "${CMAKE_C_COMPILER} -E -DMTH_CMPLX_C99_ABI -DMTH_NO_STD_MATH_HDRS -dM ${CMAKE_CURRENT_SOURCE_DIR}/../${TARGET_NAME}.h")
separate_arguments(PREPROCESSOR UNIX_COMMAND "${PREPROCESSOR}")

add_custom_command(OUTPUT ${TARGET_NAME}.i PRE_BUILD
  COMMAND ${PREPROCESSOR} > ${TARGET_NAME}.i
  DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/../${TARGET_NAME}.h)

add_custom_command(OUTPUT tmp.${TARGET_NAME}.h PRE_BUILD
  COMMAND cat ${TARGET_NAME}.i 2>&1 | grep -e "^#define __MTH_C99_CMPLX_SUFFIX" > tmp.${TARGET_NAME}.h
  DEPENDS ${TARGET_NAME}.i)

add_custom_target(${TARGET_NAME} ALL
  DEPENDS tmp.${TARGET_NAME}.h)

if(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
  list(APPEND DEFINITIONS LINUX_ELF)
endif()
include_directories(${CMAKE_CURRENT_BINARY_DIR})
set(SRCS
  fastmath.S)
libmath_add_object_library("${SRCS}" "-m64 -O3" "${DEFINITIONS}" "fastmath")
add_dependencies(fastmath mthdecls)
