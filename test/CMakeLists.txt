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

message(STATUS "Building test suite")

# Test runner infrastructure for Flang. This configures the Flang test trees
# for use by Lit, and delegates to LLVM's lit test handlers.

if (CMAKE_CFG_INTDIR STREQUAL ".")
  set(LLVM_BUILD_MODE ".")
else ()
  set(LLVM_BUILD_MODE "%(build_mode)s")
endif ()

string(REPLACE ${CMAKE_CFG_INTDIR} ${LLVM_BUILD_MODE} FLANG_TOOLS_DIR ${LLVM_RUNTIME_OUTPUT_INTDIR})

configure_lit_site_cfg(
  ${CMAKE_CURRENT_SOURCE_DIR}/lit.site.cfg.in
  ${CMAKE_CURRENT_BINARY_DIR}/lit.site.cfg
  )

#configure_lit_site_cfg(
#  ${CMAKE_CURRENT_SOURCE_DIR}/Unit/lit.site.cfg.in
#  ${CMAKE_CURRENT_BINARY_DIR}/Unit/lit.site.cfg
#  )

option(FLANG_TEST_USE_VG "Run Flang tests under Valgrind" OFF)
if(FLANG_TEST_USE_VG)
  set(FLANG_TEST_EXTRA_ARGS ${FLANG_TEST_EXTRA_ARGS} "--vg")
endif ()

option(FLANG_TEST_VERBOSE_MODE "Run Flang tests in verbose mode" OFF)
if(FLANG_TEST_VERBOSE_MODE)
  set(FLANG_TEST_EXTRA_ARGS ${FLANG_TEST_EXTRA_ARGS} "-vv")
endif ()

set(FLANG_TEST_PARAMS
  flang_site_config=${CMAKE_CURRENT_BINARY_DIR}/lit.site.cfg
  )

if( NOT FLANG_BUILT_STANDALONE )
  list(APPEND FLANG_TEST_DEPS
    llvm-config
    FileCheck count not
    llc
    llvm-bcanalyzer
    llvm-nm
    llvm-objdump
    llvm-profdata
    llvm-readobj
    llvm-symbolizer
    opt
    )

  if(TARGET llvm-lto)
    list(APPEND FLANG_TEST_DEPS llvm-lto)
  endif()
endif()

add_custom_target(flang-test-depends DEPENDS ${FLANG_TEST_DEPS})
set_target_properties(flang-test-depends PROPERTIES FOLDER "Flang tests")

add_lit_testsuite(check-flang "Running Flang regression tests"
  ${CMAKE_CURRENT_BINARY_DIR}
  #LIT ${LLVM_LIT}
  PARAMS ${FLANG_TEST_PARAMS}
  DEPENDS ${FLANG_TEST_DEPS}
  ARGS ${FLANG_TEST_EXTRA_ARGS}
  )
set_target_properties(check-flang PROPERTIES FOLDER "Flang tests")

add_lit_testsuites(FLANG ${CMAKE_CURRENT_SOURCE_DIR}
  PARAMS ${FLANG_TEST_PARAMS}
  DEPENDS ${FLANG_TEST_DEPS}
)

# Add a legacy target spelling: flang-test
add_custom_target(flang-test)
add_dependencies(flang-test check-flang)
set_target_properties(flang-test PROPERTIES FOLDER "Flang tests")
