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

set(OMPSTUB_SRC init_nomp.c  ompstubs.c)

add_flang_library(ompstub_static ${OMPSTUB_SRC})
if (MSVC)
set_property(TARGET ompstub_static PROPERTY OUTPUT_NAME libompstub)
else()
set_property(TARGET ompstub_static PROPERTY OUTPUT_NAME ompstub)
endif()

set(SHARED_LIBRARY TRUE)
add_flang_library(ompstub_shared ${OMPSTUB_SRC})
set_property(TARGET ompstub_shared PROPERTY OUTPUT_NAME ompstub)
if (MSVC)
  set_target_properties(ompstub_shared PROPERTIES WINDOWS_EXPORT_ALL_SYMBOLS TRUE)
endif()
set(SHARED_LIBRARY FALSE)

set_target_properties(ompstub_static ompstub_shared
  PROPERTIES
  ARCHIVE_OUTPUT_DIRECTORY ${FLANG_RTE_LIB_DIR}
  )

