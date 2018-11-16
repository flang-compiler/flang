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

set(FLANG_TOOLS_DIR    ${CMAKE_CURRENT_SOURCE_DIR})
set(FLANG2_SRC_DIR     ${CMAKE_CURRENT_SOURCE_DIR}/flang2exe)
set(FLANG2_DOC_SRC_DIR ${CMAKE_CURRENT_SOURCE_DIR}/docs)
set(UTILS_UPPER_DIR    ${CMAKE_CURRENT_SOURCE_DIR}/utils/upper)
set(UTILS_ILI_DIR      ${CMAKE_CURRENT_SOURCE_DIR}/utils/ilitp)
set(UTILS_ILM_DIR      ${CMAKE_CURRENT_SOURCE_DIR}/utils/ilmtp)
set(UTILS_SYMTAB_DIR   ${CMAKE_CURRENT_SOURCE_DIR}/utils/symtab)
set(UTILS_MACHAR_DIR   ${CMAKE_CURRENT_SOURCE_DIR}/utils/machar)

set(UTILS_SYMTAB_BIN_DIR  ${CMAKE_CURRENT_BINARY_DIR}/utils/symtab)
set(UTILS_ILI_BIN_DIR     ${CMAKE_CURRENT_BINARY_DIR}/utils/ilitp)
set(UTILS_ILM_BIN_DIR     ${CMAKE_CURRENT_BINARY_DIR}/utils/ilmtp)
set(UTILS_MACHAR_BIN_DIR  ${CMAKE_CURRENT_BINARY_DIR}/utils/machar)
set(UTILS_UPPER_BIN_DIR   ${CMAKE_CURRENT_BINARY_DIR}/utils/upper)
set(FLANG2_DOC_BIN_DIR    ${CMAKE_CURRENT_BINARY_DIR}/../../docs/sphinx/flang2)
set(FLANG2_INCLUDE_DIR    ${CMAKE_CURRENT_BINARY_DIR}/include)

include_directories(${FLANG2_INCLUDE_DIR})

if( ${TARGET_OS} STREQUAL "Linux" )
  if( ${TARGET_ARCHITECTURE} STREQUAL "x86_64" )
    set(X86_64 ON)
    set(LINUX86 ON)
  endif()
endif()

option(FLANG_OPENMP_GPU_NVIDIA "Enable OpenMP Accelerator Offload." OFF)
if (FLANG_OPENMP_GPU_NVIDIA)
    add_definitions("-DOMP_OFFLOAD_LLVM")
endif()

add_subdirectory(include)
add_subdirectory(utils)
add_subdirectory(flang2exe)

# Local Variables:
# mode: cmake
# End:
