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

add_executable(ilitp
  ilitp.cpp
  ${UTILS_COMMON_DIR}/utils.cpp
  )

target_compile_definitions(ilitp
  PRIVATE
  ILITP_UTIL
  PGF90=1
  TARGET_64
  )

target_include_directories(ilitp
  PRIVATE
  ${FLANG2_SRC_DIR}
  ${FLANG_INCLUDE_DIR}
  ${UTILS_COMMON_DIR}
  ) 

# Generate ILI tables

add_custom_command( 
  OUTPUT ${UTILS_ILI_BIN_DIR}/schinfo.h ${UTILS_ILI_BIN_DIR}/ilinfodf.h ${UTILS_ILI_BIN_DIR}/iliatt.h
  COMMAND ${CMAKE_BINARY_DIR}/bin/ilitp -I${UTILS_ILI_DIR} ${ARCH_DEP_ILI_DIR}/ilitp.n
  DEPENDS ilitp
          ${UTILS_ILI_DIR}/ilitp_atomic.n
          ${ARCH_DEP_ILI_DIR}/ilitp.n
          ${ARCH_DEP_ILI_DIR}/ilitp_longdouble.n
  )

add_custom_target(gen_backend_ili
  SOURCES ${UTILS_ILI_BIN_DIR}/schinfo.h ${UTILS_ILI_BIN_DIR}/ilinfodf.h ${UTILS_ILI_BIN_DIR}/iliatt.h
  )
