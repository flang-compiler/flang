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

add_definitions("-DILMTOOLBUILD")

add_executable(ilmtp
  ilmtp.cpp
  ${UTILS_COMMON_DIR}/utils.cpp
  )

target_include_directories(ilmtp
  PRIVATE
  ${UTILS_COMMON_DIR}
  ${FLANG_TOOLS_DIR}
  )

# Generate ILM tables

add_custom_command(
  OUTPUT ${UTILS_ILM_BIN_DIR}/ilmtpdf.h
         ${UTILS_ILM_BIN_DIR}/ilmtp.h
         ${UTILS_ILM_BIN_DIR}/ilmtp.n1
         ${FLANG2_DOC_BIN_DIR}/ilmtp.rst
  COMMAND ${CMAKE_BINARY_DIR}/bin/ilmtp -I${UTILS_ILI_DIR} -l scftn
                                                 ${ARCH_DEP_ILM_DIR}/ilmtp.n
                                                 ${ARCH_DEP_ILI_DIR}/ilitp.n
                                              -o ${UTILS_ILM_BIN_DIR}/ilmtpdf.h
                                                 ${UTILS_ILM_BIN_DIR}/ilmtp.h
                                              -n ${UTILS_ILM_BIN_DIR}/ilmtp.n1
                                              -s ${FLANG2_DOC_BIN_DIR}/ilmtp.rst
  DEPENDS ilmtp ${ARCH_DEP_ILM_DIR}/ilmtp.n
                ${ARCH_DEP_ILM_DIR}/ilmtp_atomic.n
                ${ARCH_DEP_ILM_DIR}/ilmtp_longdouble.n
                ${ARCH_DEP_ILI_DIR}/ilitp.n
  )

add_custom_target(gen_backend_ilm
  SOURCES ${UTILS_ILM_BIN_DIR}/ilmtpdf.h
          ${UTILS_ILM_BIN_DIR}/ilmtp.h
          ${UTILS_ILM_BIN_DIR}/ilmtp.n1
          ${FLANG2_DOC_BIN_DIR}/ilmtp.rst
  )

# Local Variables:
# mode: cmake
# End:
