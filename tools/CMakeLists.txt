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

set(FLANG_INCLUDE_DIR ${FLANG_SOURCE_DIR}/include/flang)
set(SHARED_UTILS_INCLUDE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/include)

set(TOOLS_SHARED_DIR ${CMAKE_CURRENT_SOURCE_DIR}/shared)
set(UTILS_SHARED_DIR ${CMAKE_CURRENT_SOURCE_DIR}/shared/utils)
set(UTILS_COMMON_DIR ${CMAKE_CURRENT_SOURCE_DIR}/shared/utils/common)

if( ${TARGET_ARCHITECTURE} STREQUAL "aarch64" )
  add_definitions(-DLLVM_ENABLE_FFI=false)
endif()

add_subdirectory(shared)
add_subdirectory(flang1)
add_subdirectory(flang2)
