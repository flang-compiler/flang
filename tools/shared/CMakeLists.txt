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

set(SHARED_SOURCES 
  ${CMAKE_CURRENT_SOURCE_DIR}/ccffinfo.c
  ${CMAKE_CURRENT_SOURCE_DIR}/direct.c
  ${CMAKE_CURRENT_SOURCE_DIR}/error.c
  ${CMAKE_CURRENT_SOURCE_DIR}/go.c
  ${CMAKE_CURRENT_SOURCE_DIR}/ilidir.c
  ${CMAKE_CURRENT_SOURCE_DIR}/llmputil.c
  ${CMAKE_CURRENT_SOURCE_DIR}/mall.c
  ${CMAKE_CURRENT_SOURCE_DIR}/miscutil.c
  ${CMAKE_CURRENT_SOURCE_DIR}/pragma.c
  ${CMAKE_CURRENT_SOURCE_DIR}/rtlRtns.c
  ${CMAKE_CURRENT_SOURCE_DIR}/salloc.c
  ${CMAKE_CURRENT_SOURCE_DIR}/mach.c
  ${CMAKE_CURRENT_SOURCE_DIR}/nmeutil.c
  PARENT_SCOPE
)

include_directories($(CMAKE_CURRENT_SOURCE_DIR))
