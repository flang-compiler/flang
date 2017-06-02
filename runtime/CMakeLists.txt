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

add_definitions(
 -DMAXCPUS=256 
 -DMAXCPUSL=8 
 -DMAXCPUSR=8 
 -DTARGET_LINUX 
 -DLINUX 
 -DPGF90 
 -DPGFLANG
 -DNATIVE_FPCVT
 -DPGI_LITTLE_ENDIAN
)

if( ${TARGET_ARCHITECTURE} STREQUAL "x86_64" )
  add_definitions(
   -DTARGET_X8664
   -DTARGET_64
  )
elseif( ${TARGET_ARCHITECTURE} STREQUAL "aarch64" )
  add_definitions(
   -DTARGET_LLVM_ARM64
   -DTARGET_LINUX_ARM
   -DTARGET_64
  )
elseif( ${TARGET_ARCHITECTURE} STREQUAL "ppc64le" )
  add_definitions(
    -DTARGET_LINUX_POWER
    -DLINUX_POWER
    -DTARGET_64
  )
endif()

include_directories(${CMAKE_CURRENT_SOURCE_DIR}/include)

add_subdirectory(ompstub)

add_subdirectory(flangrti)
add_subdirectory(flang)
add_subdirectory(flangmain)
