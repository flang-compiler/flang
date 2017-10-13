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

add_executable(machar
  ${UTILS_SHARED_DIR}/machar.cpp
  ${UTILS_COMMON_DIR}/utils.cpp
  )

target_include_directories(machar
  PRIVATE
  ${FLANG_INCLUDE_DIR}
  ${UTILS_COMMON_DIR}
  )

target_compile_definitions(machar
  PRIVATE
  FE90
  )

# Generate Machar headers

add_custom_command(
  OUTPUT ${UTILS_MACHAR_BIN_DIR}/machar.h
         ${UTILS_MACHAR_BIN_DIR}/machardf.h
         ${FLANG1_DOC_BIN_DIR}/machar.rst
  COMMAND ${CMAKE_BINARY_DIR}/bin/machar ${UTILS_MACHAR_DIR}/machar.n
                                         ${UTILS_SYMTAB_BIN_DIR}/symtab.h
                                      -o ${UTILS_MACHAR_BIN_DIR}/machar.h
                                         ${UTILS_MACHAR_BIN_DIR}/machardf.h
                                      -s ${FLANG1_DOC_BIN_DIR}/machar.rst
  DEPENDS gen_frontend_symtab machar ${UTILS_MACHAR_DIR}/machar.n ${UTILS_SYMTAB_BIN_DIR}/symtab.h
  )

add_custom_target(gen_frontend_machar
  SOURCES ${UTILS_MACHAR_BIN_DIR}/machar.h
          ${UTILS_MACHAR_BIN_DIR}/machardf.h
          ${FLANG1_DOC_BIN_DIR}/machar.rst
  )

# Local Variables:
# mode: cmake
# End:
