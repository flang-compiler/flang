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

add_executable(astutil 
  astutil.c
  ${UTILS_COMMON_DIR}/utils.cpp
  )

target_include_directories(astutil
  PRIVATE
  ${FLANG_INCLUDE_DIR} 
  ${UTILS_COMMON_DIR}
  ${TOOLS_SHARED_DIR}
  ${FLANG1_SRC_DIR}
  ${FLANG1_INCLUDE_DIR}
  )

# Generate AST headers

add_custom_command( 
  OUTPUT ${UTILS_AST_BIN_DIR}/ast.out.n ${UTILS_AST_BIN_DIR}/ast.h ${UTILS_AST_BIN_DIR}/astdf.h
  COMMAND ${CMAKE_BINARY_DIR}/bin/astutil ${UTILS_AST_DIR}/ast.n ${UTILS_AST_DIR}/ast.in.h ${UTILS_SYMTAB_BIN_DIR}/ast.d ${UTILS_SYMTAB_BIN_DIR}/astdf.d -o -n ${UTILS_AST_BIN_DIR}/ast.out.n ${UTILS_AST_BIN_DIR}/ast.h ${UTILS_AST_BIN_DIR}/astdf.h
  DEPENDS gen_frontend_symini astutil ${UTILS_AST_DIR}/ast.n ${UTILS_AST_DIR}/ast.in.h ${UTILS_SYMTAB_BIN_DIR}/ast.d ${UTILS_SYMTAB_BIN_DIR}/astdf.d
  )

add_custom_target(gen_frontend_ast
  SOURCES ${UTILS_AST_BIN_DIR}/ast.out.n ${UTILS_AST_BIN_DIR}/ast.h ${UTILS_AST_BIN_DIR}/astdf.h
  )
