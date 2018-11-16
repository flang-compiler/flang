#
# Copyright (c) 2017-2018, NVIDIA CORPORATION.  All rights reserved.
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


set(FLANG1_SRC_DIR       ${CMAKE_CURRENT_SOURCE_DIR}/flang1exe)
set(FLANG1_DOC_SRC_DIR   ${CMAKE_CURRENT_SOURCE_DIR}/docs)
set(UTILS_SYMTAB_DIR     ${CMAKE_CURRENT_SOURCE_DIR}/utils/symtab)
set(UTILS_AST_DIR        ${CMAKE_CURRENT_SOURCE_DIR}/utils/ast)
set(UTILS_MACHAR_DIR     ${CMAKE_CURRENT_SOURCE_DIR}/utils/machar)
set(UTILS_PARSETABLE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/utils/prstab)

set(UTILS_AST_BIN_DIR        ${CMAKE_CURRENT_BINARY_DIR}/utils/ast)
set(UTILS_SYMTAB_BIN_DIR     ${CMAKE_CURRENT_BINARY_DIR}/utils/symtab)
set(UTILS_MACHAR_BIN_DIR     ${CMAKE_CURRENT_BINARY_DIR}/utils/machar)
set(UTILS_PARSETABLE_BIN_DIR ${CMAKE_CURRENT_BINARY_DIR}/utils/prstab)
set(FLANG1_DOC_BIN_DIR       ${CMAKE_CURRENT_BINARY_DIR}/../../docs/sphinx/flang1)
set(FLANG1_INCLUDE_DIR       ${CMAKE_CURRENT_BINARY_DIR}/include)

include_directories(${FLANG1_INCLUDE_DIR})

option(FLANG_OPENMP_GPU_NVIDIA "Enable OpenMP Accelerator Offload." OFF)
if (FLANG_OPENMP_GPU_NVIDIA)
    add_definitions("-DOMP_OFFLOAD_LLVM")
endif()

add_subdirectory(include)
add_subdirectory(utils)
add_subdirectory(flang1exe)

# Local Variables:
# mode: cmake
# End:
