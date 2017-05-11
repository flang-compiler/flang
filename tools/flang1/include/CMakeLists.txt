
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

# Generate platform header

configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/platform.h.in
  ${CMAKE_CURRENT_BINARY_DIR}/platform.h
  @ONLY)

# Generate error message headers

add_custom_command(
  OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/errmsgdf.h
         ${CMAKE_CURRENT_BINARY_DIR}/errmsg.n
         ${FLANG1_DOC_BIN_DIR}/errmsg.rst
  COMMAND ${CMAKE_BINARY_DIR}/bin/errmsggen ${FLANG_INCLUDE_DIR}/Error/errmsg-in.n
                                            ${FLANG_INCLUDE_DIR}/Error/errmsg-common.n
                                            ${FLANG_INCLUDE_DIR}/Error/errmsg-ipa.n
                                         -o ${CMAKE_CURRENT_BINARY_DIR}/errmsgdf.h
                                         -e ${CMAKE_CURRENT_BINARY_DIR}/errmsg.n
                                         -s ${FLANG1_DOC_BIN_DIR}/errmsg.rst
  DEPENDS errmsggen ${FLANG_INCLUDE_DIR}/Error/errmsg-in.n
                    ${FLANG_INCLUDE_DIR}/Error/errmsg-common.n
                    ${FLANG_INCLUDE_DIR}/Error/errmsg-ipa.n
  )

add_custom_target(gen_frontend_error_headers
  SOURCES ${CMAKE_CURRENT_BINARY_DIR}/errmsgdf.h
          ${CMAKE_CURRENT_BINARY_DIR}/errmsg.n
          ${FLANG1_DOC_BIN_DIR}/errmsg.rst
  )

# Local Variables:
# mode: cmake
# End:
