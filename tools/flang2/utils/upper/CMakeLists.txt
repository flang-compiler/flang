#
# Copyright (c) 2017, NVIDIA CORPORATION.  All rights reserved.
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

set(ENV{LC_ALL} "C")

add_executable(upperl
  upperl.c
  )


# Generate upper tables
file(STRINGS "${UTILS_UPPER_DIR}/upperilm.in" UPPERILM_H_CONTENTS)
list(SORT UPPERILM_H_CONTENTS)
set(UPPERILM_H_CONTENTS_SORTED "")
foreach(Line ${UPPERILM_H_CONTENTS})
  # Don't modify the line if it contains #local at the end.
  string(SUBSTRING "${Line}" 0 1 FIRST_CHAR)
  if(NOT "${FIRST_CHAR}" STREQUAL "#")
    set(UPPERILM_H_CONTENTS_SORTED "${UPPERILM_H_CONTENTS_SORTED}${Line}\n")
  endif()
endforeach()
file(WRITE ${UTILS_UPPER_BIN_DIR}/upperilm.sort "${UPPERILM_H_CONTENTS_SORTED}")

add_custom_command( 
  OUTPUT ${UTILS_UPPER_BIN_DIR}/upperilm.h
  COMMAND ${CMAKE_BINARY_DIR}/bin/upperl ${UTILS_UPPER_BIN_DIR}/upperilm.sort ${UTILS_UPPER_BIN_DIR}/upperilm.h
  DEPENDS upperl ${UTILS_UPPER_DIR}/upperilm.in
  )

add_custom_target(gen_backend_upper
  SOURCES ${UTILS_UPPER_BIN_DIR}/upperilm.h
  )
