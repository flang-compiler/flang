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

# Build lr and prodstr

add_executable(lr
  lrutils.c
  prstab1.c
  prstab2.c
  prstab3.c
  prstab4.c
  )

add_executable(prodstr
  prodstr.c
  )

# Generate parse tables

add_custom_command(
  OUTPUT ${UTILS_PARSETABLE_BIN_DIR}/gramdf.h ${UTILS_PARSETABLE_BIN_DIR}/gramsm.h ${UTILS_PARSETABLE_BIN_DIR}/gramtk.h ${UTILS_PARSETABLE_BIN_DIR}/proddf.h ${UTILS_PARSETABLE_BIN_DIR}/tokdf.h ${UTILS_PARSETABLE_BIN_DIR}/gramnt.h
  # Copy to current build directory, otherwise header will be generated in
  # original directory
  COMMAND ${CMAKE_COMMAND} -E copy ${UTILS_PARSETABLE_DIR}/gram.txt ${UTILS_PARSETABLE_BIN_DIR}
  COMMAND ${CMAKE_COMMAND} -E copy ${UTILS_PARSETABLE_DIR}/gram.tki ${UTILS_PARSETABLE_BIN_DIR}
  COMMAND ${CMAKE_BINARY_DIR}/bin/lr gram.txt -semant -tokens -list -data -runoff
  COMMAND ${CMAKE_BINARY_DIR}/bin/prodstr gram.txt
  DEPENDS ${UTILS_PARSETABLE_DIR}/gram.tki ${UTILS_PARSETABLE_DIR}/gram.txt
  )

add_custom_target(gen_frontend_parsetable
  SOURCES ${UTILS_PARSETABLE_BIN_DIR}/gramdf.h ${UTILS_PARSETABLE_BIN_DIR}/gramsm.h ${UTILS_PARSETABLE_BIN_DIR}/gramtk.h ${UTILS_PARSETABLE_BIN_DIR}/proddf.h ${UTILS_PARSETABLE_BIN_DIR}/tokdf.h ${UTILS_PARSETABLE_BIN_DIR}/gramnt.h
  )

add_dependencies(gen_frontend_parsetable lr)
add_dependencies(gen_frontend_parsetable prodstr)
