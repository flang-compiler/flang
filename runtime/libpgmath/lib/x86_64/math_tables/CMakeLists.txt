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

get_property(FLAGS GLOBAL PROPERTY "FLAGS_X8664_L1")
get_property(DEFINITIONS GLOBAL PROPERTY "DEFINITIONS_X8664_L2")

set(SRCS
  mth_acosdefs.h
  mth_asindefs.h
  mth_atandefs.h
  mth_atan2defs.h
  mth_cosdefs.h
  mth_sindefs.h
  mth_tandefs.h
  mth_coshdefs.h
  mth_sinhdefs.h
  mth_tanhdefs.h
  mth_expdefs.h
  mth_logdefs.h
  mth_log10defs.h
  mth_powdefs.h
  mth_powidefs.h
  mth_sincosdefs.h
  mth_divdefs.h
  mth_sqrtdefs.h
  mth_moddefs.h)

set(NEW_SRCS)
foreach(file ${SRCS})
  list(APPEND NEW_SRCS ${CMAKE_CURRENT_SOURCE_DIR}/${file})
endforeach()
set(SRCS ${NEW_SRCS})
set(DEPENDENCIES "${SRCS}")

string(REPLACE ";" " -D" DEFINITIONS "${DEFINITIONS}")
set(DEFINITIONS "-D${DEFINITIONS}")
string(REPLACE ";" " " SRCS "${SRCS}")
list(APPEND PREPROCESSOR "${CMAKE_C_COMPILER} -E ${DEFINITIONS} ${FLAGS} ${SRCS}")
separate_arguments(PREPROCESSOR UNIX_COMMAND "${PREPROCESSOR}")

# Generate tmp-mth_alldefs.h
set(TARGET_NAME "tmp-mth_alldefs")
add_custom_command(OUTPUT ${TARGET_NAME}.i PRE_BUILD
  COMMAND ${PREPROCESSOR} > ${TARGET_NAME}.i
  DEPENDS "${DEPENDENCIES}")

add_custom_command(OUTPUT ${TARGET_NAME}.check PRE_BUILD
  COMMAND sh "${LIBPGMATH_TOOLS_DIR}/tmp-mth-check.sh" ${TARGET_NAME}.i
  COMMAND touch ${TARGET_NAME}.check
  DEPENDS "${TARGET_NAME}.i")

add_custom_command(OUTPUT ${TARGET_NAME}.h PRE_BUILD
  COMMAND sh "${LIBPGMATH_TOOLS_DIR}/${TARGET_NAME}.sh" ${TARGET_NAME}.i ${TARGET_NAME}.h
  DEPENDS "${TARGET_NAME}.check")

add_custom_target(${TARGET_NAME} ALL
  DEPENDS "${TARGET_NAME}.h")
