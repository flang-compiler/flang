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

add_executable(n2rst
  ${UTILS_SHARED_DIR}/n2rst.cpp
  ${UTILS_COMMON_DIR}/utils.cpp
)

target_include_directories(n2rst
  PRIVATE
  ${UTILS_COMMON_DIR}
  )

# Convert nroff documentation to Sphinx format.

file(MAKE_DIRECTORY ${FLANG2_DOC_BIN_DIR})

add_custom_command(
  OUTPUT ${FLANG2_DOC_BIN_DIR}/coding.rst
         ${FLANG2_DOC_BIN_DIR}/controller.rst
         ${FLANG2_DOC_BIN_DIR}/error.rst
         ${FLANG2_DOC_BIN_DIR}/expander.rst
         ${FLANG2_DOC_BIN_DIR}/fin.rst
         ${FLANG2_DOC_BIN_DIR}/ili.rst
         ${FLANG2_DOC_BIN_DIR}/ilitp.rst
         ${FLANG2_DOC_BIN_DIR}/ilm.rst
         ${FLANG2_DOC_BIN_DIR}/intro.rst
         ${FLANG2_DOC_BIN_DIR}/register.rst
  COMMAND ${CMAKE_BINARY_DIR}/bin/n2rst -v ${FLANG2_DOC_SRC_DIR}/coding.n
                                           ${FLANG2_DOC_SRC_DIR}/controller.n
                                           ${FLANG2_DOC_SRC_DIR}/error.n
                                           ${FLANG2_DOC_SRC_DIR}/expander.n
                                           ${FLANG2_DOC_SRC_DIR}/fin.n
                                           ${FLANG2_DOC_SRC_DIR}/ili.n
                                           ${ARCH_DEP_ILI_DIR}/ilitp.n
                                           ${FLANG2_DOC_SRC_DIR}/ilm.n
                                           ${FLANG2_DOC_SRC_DIR}/intro.n
                                           ${FLANG2_DOC_SRC_DIR}/register.n
  WORKING_DIRECTORY ${FLANG2_DOC_BIN_DIR}
  DEPENDS n2rst ${FLANG2_DOC_SRC_DIR}/coding.n
                ${FLANG2_DOC_SRC_DIR}/controller.n
                ${FLANG2_DOC_SRC_DIR}/error.n
                ${FLANG2_DOC_SRC_DIR}/expander.n
                ${FLANG2_DOC_SRC_DIR}/fin.n
                ${FLANG2_DOC_SRC_DIR}/ili.n
                ${ARCH_DEP_ILI_DIR}/ilitp.n
                ${FLANG2_DOC_SRC_DIR}/ilm.n
                ${FLANG2_DOC_SRC_DIR}/intro.n
                ${FLANG2_DOC_SRC_DIR}/register.n
  )

add_custom_target(gen_sphinx_docs
  SOURCES ${FLANG2_DOC_BIN_DIR}/coding.rst
          ${FLANG2_DOC_BIN_DIR}/controller.rst
          ${FLANG2_DOC_BIN_DIR}/error.rst
          ${FLANG2_DOC_BIN_DIR}/expander.rst
          ${FLANG2_DOC_BIN_DIR}/fin.rst
          ${FLANG2_DOC_BIN_DIR}/ili.rst
          ${FLANG2_DOC_BIN_DIR}/ilitp.rst
          ${FLANG2_DOC_BIN_DIR}/ilm.rst
          ${FLANG2_DOC_BIN_DIR}/intro.rst
          ${FLANG2_DOC_BIN_DIR}/register.rst
  )

# Local Variables:
# mode: cmake
# End:
