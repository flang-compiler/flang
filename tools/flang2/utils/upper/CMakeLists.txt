#
# Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
# See https://llvm.org/LICENSE.txt for license information.
# SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
#

set(ENV{LC_ALL} "C")

add_executable(upperl
  upperl.c
  )

# Generate upper tables

add_custom_command( 
  OUTPUT ${UTILS_UPPER_BIN_DIR}/upperilm.h
  COMMAND LC_ALL=C sort ${UTILS_UPPER_DIR}/upperilm.in | grep -v "^ *\#" > ${UTILS_UPPER_BIN_DIR}/upperilm.sort
  COMMAND ${CMAKE_BINARY_DIR}/bin/upperl ${UTILS_UPPER_BIN_DIR}/upperilm.sort ${UTILS_UPPER_BIN_DIR}/upperilm.h
  DEPENDS upperl ${UTILS_UPPER_DIR}/upperilm.in
  )

add_custom_target(gen_backend_upper
  SOURCES ${UTILS_UPPER_BIN_DIR}/upperilm.h
  )
