#
# Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
# See https://llvm.org/LICENSE.txt for license information.
# SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
#

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
  COMMAND upperl ${UTILS_UPPER_BIN_DIR}/upperilm.sort ${UTILS_UPPER_BIN_DIR}/upperilm.h
  DEPENDS upperl ${UTILS_UPPER_DIR}/upperilm.in
  )

add_custom_target(gen_backend_upper
  SOURCES ${UTILS_UPPER_BIN_DIR}/upperilm.h
  )
