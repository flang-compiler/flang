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

add_library(scutil
 default-fperror.c
 host-fp-folding.c
 int128.c
 legacy-folding-api.c
 legacy-ints.c
 lockfile.c
 path-utils.c
 pgnewfil.c
 cpu-stopwatch.c
)

target_include_directories(scutil
  PRIVATE
  ${FLANG_INCLUDE_DIR}
  ${CMAKE_CURRENT_SOURCE_DIR}
  )

set_target_properties(scutil
  PROPERTIES
  PREFIX ""
  ARCHIVE_OUTPUT_DIRECTORY ${FLANG_LIB_DIR}
  C_STANDARD 99
)

# Clang does not support FENV_ACCESS
set_source_files_properties(
  host-fp-folding.c
  PROPERTIES
  COMPILE_FLAGS "${CMAKE_C_FLAGS} -Wno-unknown-pragmas"
)
