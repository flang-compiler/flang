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
get_property(DEFINITIONS GLOBAL PROPERTY "DEFINITIONS_L2")
get_property(FLAGS GLOBAL PROPERTY "FLAGS_L2")

set(SRCS)

# Set source files, compiler flags and definitions
if(${CMAKE_SYSTEM_PROCESSOR} MATCHES "x86_64")
  if(${CMAKE_SYSTEM_NAME} MATCHES "Linux|Darwin")
    set(SRCS
      sdexp.cpp
      ssexp.cpp
      vdexp2.cpp
      vdexp4.cpp
      vsexp4.cpp
      vsexp8.cpp)
  endif()
  libmath_add_object_library("${SRCS}" "${FLAGS}" "${DEFINITIONS}" "")
elseif(${CMAKE_SYSTEM_PROCESSOR} MATCHES "ppc64le")
  set(SRCS
    ssexp.cpp)
  set_property(SOURCE ssexp.cpp APPEND PROPERTY COMPILE_DEFINITIONS VINT=)
  libmath_add_object_library("${SRCS}" "${FLAGS}" "${DEFINITIONS}" "")

  set(NEWDEFS ${DEFINITIONS})
  string(REPLACE ";" " -D" NEWDEFS "${NEWDEFS}")
  set(NEWDEFS "-D${NEWDEFS}")

  libmath_add_object_library_asm("sdexp" "${NEWDEFS} -DVINT=long" "${FLAGS}")
  libmath_add_object_library_asm("vdexp2" "${NEWDEFS} -DVINT=long" "${FLAGS}")
  libmath_add_object_library_asm("vsexp4" "${NEWDEFS} -DVINT=" "${FLAGS}")
endif()
