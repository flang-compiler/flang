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

cmake_minimum_required(VERSION 3.4.3)

if(POLICY CMP0042)
  cmake_policy(SET CMP0042 NEW) # Set MACOSX_RPATH=YES by default
endif()
if(POLICY CMP0022)
  cmake_policy(SET CMP0022 NEW) # Required when interacting with LLVM and Clang
endif()

# Add path for custom modules
set(CMAKE_MODULE_PATH
  "${CMAKE_CURRENT_SOURCE_DIR}/cmake"
  "${CMAKE_CURRENT_SOURCE_DIR}/cmake/modules"
  ${CMAKE_MODULE_PATH})

set(PACKAGE_NAME libpgm)

# Standalone build or part of LLVM?
set(LIBPGM_STANDALONE_BUILD FALSE)
if (CMAKE_SOURCE_DIR STREQUAL CMAKE_CURRENT_SOURCE_DIR)
  project(pgm C CXX ASM)

  set(LIBPGM_STANDALONE_BUILD TRUE)
endif()

if (LIBPGM_STANDALONE_BUILD)
  include(FindPythonInterp)
  if( NOT PYTHONINTERP_FOUND )
    message(WARNING "Failed to find python interpreter. "
                    "The libmath test suite will be disabled.")
    set(LLVM_INCLUDE_TESTS OFF)
  endif()
endif()

if(CMAKE_C_COMPILER_ID STREQUAL "GNU" AND ${CMAKE_SYSTEM_PROCESSOR} MATCHES "x86_64|AMD64")
  if(CMAKE_C_COMPILER_VERSION VERSION_LESS "7.2.0")
    message(FATAL_ERROR "Found gcc at " ${CMAKE_C_COMPILER} " version " ${CMAKE_C_COMPILER_VERSION} ", but version 7.2.0 or newer required.")
  endif()
# elseif(CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
#   if(CMAKE_CXX_COMPILER_VERSION VERSION_LESS "your.required.msvc.version")
#     message(FATAL_ERROR "Insufficient msvc version")
#   endif()
# elseif(...)
# # etc.
endif()

# Needs to be changed to support cross-compilation
include(GetHostTriple)
get_host_triple(LIBPGM_HOST_TRIPLE)

# Setting directory names
set(LIBPGM_BASE_DIR ${CMAKE_CURRENT_SOURCE_DIR})
set(LIBPGM_SRC_DIR ${LIBPGM_BASE_DIR}/lib)
set(LIBPGM_TOOLS_DIR ${LIBPGM_BASE_DIR}/tools)
set(LIBPGM_BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR})
set(LIBPGM_RUNTIME_PATH ${CMAKE_BINARY_DIR}/lib)
set(LIBPGM_LIBRARY_NAME pgmath)
set(LIBPGM_RTL lib${LIBPGM_LIBRARY_NAME}.so)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

# Require out of source build.
include(MacroEnsureOutOfSourceBuild)
MACRO_ENSURE_OUT_OF_SOURCE_BUILD(
 "${PROJECT_NAME} requires an out of source build. Please create a separate
 build directory and run 'cmake /path/to/${PROJECT_NAME} [options]' there."
 )

# Support functions
include(LibmathUtils)

# Setup Source Code And Tests
add_subdirectory(lib)
add_subdirectory(test)
