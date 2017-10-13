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

# Generate symbol table headers

add_custom_command(
  OUTPUT ${UTILS_SYMTAB_BIN_DIR}/symtab.out.n
         ${UTILS_SYMTAB_BIN_DIR}/symtab.h
         ${UTILS_SYMTAB_BIN_DIR}/symtabdf.h
         ${UTILS_SYMTAB_BIN_DIR}/symnames.h
         ${FLANG2_DOC_BIN_DIR}/symtab.rst
  COMMAND ${CMAKE_BINARY_DIR}/bin/fsymutil ${CMAKE_CURRENT_SOURCE_DIR}/symtab.n
                                           ${CMAKE_CURRENT_SOURCE_DIR}/symtab.in.h
                                     -o -n ${UTILS_SYMTAB_BIN_DIR}/symtab.out.n
                                           ${UTILS_SYMTAB_BIN_DIR}/symtab.h
                                           ${UTILS_SYMTAB_BIN_DIR}/symtabdf.h
                                           ${UTILS_SYMTAB_BIN_DIR}/symnames.h
                                        -s ${FLANG2_DOC_BIN_DIR}/symtab.rst
  DEPENDS fsymutil ${CMAKE_CURRENT_SOURCE_DIR}/symtab.n ${CMAKE_CURRENT_SOURCE_DIR}/symtab.in.h
  )

add_custom_target(gen_backend_symtab
  SOURCES ${UTILS_SYMTAB_BIN_DIR}/symtab.out.n
          ${UTILS_SYMTAB_BIN_DIR}/symtab.h
          ${UTILS_SYMTAB_BIN_DIR}/symtabdf.h
          ${UTILS_SYMTAB_BIN_DIR}/symnames.h
          ${FLANG2_DOC_BIN_DIR}/symtab.rst
  )

add_custom_command(
  OUTPUT ${UTILS_SYMTAB_BIN_DIR}/pd.h
         ${UTILS_SYMTAB_BIN_DIR}/syminidf.h
         ${FLANG2_DOC_BIN_DIR}/symini.rst
  COMMAND ${CMAKE_BINARY_DIR}/bin/fsymini ${UTILS_SYMTAB_DIR}/symini_ftn.n
                                          ${ARCH_DEP_ILM_DIR}/ilmtp.n
                                       -o ${UTILS_SYMTAB_BIN_DIR}/syminidf.h
                                          ${UTILS_SYMTAB_BIN_DIR}/pd.h
                                       -s ${FLANG2_DOC_BIN_DIR}/symini.rst
  DEPENDS fsymini ${UTILS_SYMTAB_DIR}/symini_ftn.n ${ARCH_DEP_ILM_DIR}/ilmtp.n
  )

add_custom_target(gen_backend_symini
  SOURCES ${UTILS_SYMTAB_BIN_DIR}/pd.h
          ${UTILS_SYMTAB_BIN_DIR}/syminidf.h
          ${FLANG2_DOC_BIN_DIR}/symini.rst
  )

add_executable(fsymutil
  ${UTILS_SHARED_DIR}/symutil.cpp
  ${UTILS_COMMON_DIR}/utils.cpp
  )

add_dependencies(fsymutil gen_backend_error_headers)

target_include_directories(fsymutil
  BEFORE
  PRIVATE
  ${UTILS_SHARED_DIR}
  ${FLANG_INCLUDE_DIR}
  ${UTILS_COMMON_DIR}
  )

add_executable(fsymini
  ${UTILS_SHARED_DIR}/symacc.c
  ${UTILS_SHARED_DIR}/symacc.h
  symini.cpp
  ${UTILS_COMMON_DIR}/utils.h
  ${UTILS_COMMON_DIR}/utils.cpp
  ${UTILS_SYMTAB_BIN_DIR}/symtab.h
  ${UTILS_SYMTAB_BIN_DIR}/symtabdf.h
  ${UTILS_SYMTAB_BIN_DIR}/symnames.h
  )

add_dependencies(fsymini
  gen_backend_symtab
  )

target_compile_definitions(fsymini
  PRIVATE
  UTILSYMTAB
  )

target_include_directories(fsymini
  BEFORE
  PRIVATE
  ${SHARED_UTILS_INCLUDE_DIR}/symtab
  ${FLANG_INCLUDE_DIR}
  ${UTILS_SHARED_DIR}
  ${UTILS_COMMON_DIR}
  ${CMAKE_CURRENT_SOURCE_DIR}
  ${UTILS_SYMTAB_BIN_DIR}
  )

# Local Variables:
# mode: cmake
# End:
