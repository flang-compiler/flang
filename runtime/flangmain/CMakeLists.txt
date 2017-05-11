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

enable_language(C)

# Entry point for Fortran programs

add_flang_library(flangmain
  STATIC
  flangmain.c
  )

set_property(
 SOURCE flangmain.c
 PROPERTY COMPILE_DEFINITIONS 
  TM_I8
  PGF90
  KANJI
 )

set_property(
  SOURCE flangmain.c PROPERTY
  COMPILE_FLAGS -Wno-implicit-function-declaration
  )

set_target_properties(flangmain
  PROPERTIES
  ARCHIVE_OUTPUT_DIRECTORY ${FLANG_RTE_LIB_DIR}
  )
  
# install step should move from FLANG_LIB_DIR to ???
