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

if( ${TARGET_ARCHITECTURE} STREQUAL "x86_64" )
  set(ARCH_DEP_FILES
    x86_64-Linux/ll_abi.c
  )
elseif( ${TARGET_ARCHITECTURE} STREQUAL "aarch64" )
  set(ARCH_DEP_FILES
      aarch64-Linux/ll_abi.c
  )
elseif( ${TARGET_ARCHITECTURE} STREQUAL "ppc64le" )
  set(ARCH_DEP_FILES
    ppc64le-Linux/ll_abi.c
  )
endif()


set(SOURCES
  ${ARCH_DEP_FILES}
  bihutil.c
  dinitutl.c
  dinit.c
  dtypeutl.c
  fenddf.c
  ilmutil.c
  listing.c
  mwd.c
  semsym.c
  semutil0.c
  symtab.c
  symacc.c
  upper.c
  xref.c
  expreg.c
  exputil.c
  regutil.c
  expdf.c
  ili-rewrite.c
  ilidf.c
  iliutil.c
  iltutil.c
  expand.c
  expatomics.c
  expsmp.c
  exp_ftn.c
  exp_fvec.c
  exp_rte.c
  outliner.c
  rmsmove.c
  scope.c
  rbtree.c
  lili2llvm.c
  ll_dbgutl.c
  dwarf_names.c
  llassem.c
  llassem_common.c
  asm_anno.c
  main.c
  version.c
  cgmain.c
  fastset.c
  lldebug.c
  llutil.c
  ll_ftn.c
  ll_structure.c
  ll_write.c
  ll_builder.c
  llopt.c
  llsched.c
  machreg.c
  kmpcutil.c
  verify.c
  kmpcutil.h
  )

set(COMMON_DEFS
  MMD
  NOVECTORIZE
  )

set(COMPILE_OPTS
  -std=c11
  )

set(INCLUDE_DIRS
  ${TOOLS_SHARED_DIR}
  ${FLANG_INCLUDE_DIR}
  ${FLANG_SOURCE_DIR}/lib/scutil
  ${CMAKE_CURRENT_SOURCE_DIR}
  ${CMAKE_CURRENT_BINARY_DIR}
  ${CMAKE_CURRENT_SOURCE_DIR}/${TARGET_ARCHITECTURE}-${TARGET_OS}
  ${UTILS_SYMTAB_BIN_DIR}        # Symbol table headers
  ${UTILS_ILI_BIN_DIR}           # ILI IR headers
  ${UTILS_ILM_BIN_DIR}           # ILM IR headers
  ${UTILS_MACHAR_BIN_DIR}        # ILM IR headers
  ${UTILS_UPPER_BIN_DIR}         # `Upper' headers
  ${UTILS_SHARED_DIR}
  )

add_flang_executable(flang2
  ${SOURCES} ${SHARED_SOURCES}
  )

target_compile_definitions(flang2
  PRIVATE
  ${COMMON_DEFS}
  )

target_compile_options(flang2
  PRIVATE
  ${COMPILE_OPTS}
  )

  
target_link_libraries(flang2
  flangArgParser
  scutil
  )
  
if (NOT MSVC)
target_link_libraries(flang2  m)
endif()

add_dependencies(flang2
  gen_backend_error_headers # Error message headers
  gen_backend_symtab        # Symbol table headers
  gen_backend_symini        # Symbol table (symini) headers
  gen_backend_ili           # ILI IR headers
  gen_backend_ilm           # ILM IR headers
  gen_backend_machar        # Machar headers
  gen_backend_upper         # `Upper' headers
  )

if (FLANG_INCLUDE_DOCS)
  add_dependencies(flang2
    gen_sphinx_docs
    )
endif()

target_include_directories(flang2
  PRIVATE
  ${INCLUDE_DIRS}
  )

# Install flang2 executable
install(TARGETS flang2
  RUNTIME DESTINATION bin)

# Local Variables:
# mode: cmake
# End:
