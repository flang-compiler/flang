#
# Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
# See https://llvm.org/LICENSE.txt for license information.
# SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
#

set (RUNTIME_SHARED_DIR ${CMAKE_CURRENT_SOURCE_DIR}/shared)

if( ${TARGET_OS} STREQUAL "Darwin" )
  add_definitions(
   -DTARGET_OSX
   -DTARGET_OSX_X8664
   -DOSX86
  )
else()
  add_definitions(
   -DTARGET_LINUX
   -DLINUX
  )
endif()

add_definitions(
 -DMAXCPUS=256 
 -DMAXCPUSL=8 
 -DMAXCPUSR=8 
 -DTARGET_LLVM 
 -DPGF90 
 -DPGFLANG
 -DNATIVE_FPCVT
 -DPGI_LITTLE_ENDIAN
 -DINLINE_MEMOPS
)

if( ${TARGET_ARCHITECTURE} STREQUAL "x86_64" )
  add_definitions(
   -DTARGET_X8664
   -DTARGET_LINUX_X8664
  )
elseif( ${TARGET_ARCHITECTURE} STREQUAL "aarch64" )
  add_definitions(
   -DTARGET_LLVM_ARM64
   -DTARGET_LINUX_ARM
  )
elseif( ${TARGET_ARCHITECTURE} STREQUAL "ppc64le" )
  add_definitions(
    -DTARGET_LINUX_POWER
    -DLINUX_POWER
  )
endif()

include_directories(${CMAKE_CURRENT_SOURCE_DIR}/include)

add_subdirectory(ompstub)

add_subdirectory(flangrti)
add_subdirectory(flang)
add_subdirectory(flangmain)
