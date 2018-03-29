#
# Copyright (c) 2018, NVIDIA CORPORATION.  All rights reserved.
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

# Set compiler definitions and flags
get_property(DEFINITIONS GLOBAL PROPERTY "DEFINITIONS_X8664_L2")
get_property(FLAGS GLOBAL PROPERTY "FLAGS_X8664_L2")
if(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
  set(FLAGS "${FLAGS}-mtune=knl -march=knl ")
elseif(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
  set(FLAGS "${FLAGS}-mavx512f -mfma ")
endif()

# Set source files
set(SRCS
 vdpow8.c
 vdpow_mask.c
 vspow16.c
 vspow_mask.c)
libmath_add_object_library("${SRCS}" "${FLAGS}" "${DEFINITIONS}" "")
