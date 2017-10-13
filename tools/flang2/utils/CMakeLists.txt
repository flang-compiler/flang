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

set(ARCH_DEP_ILI_DIR  ${UTILS_ILI_DIR}/${TARGET_ARCHITECTURE})
set(ARCH_DEP_ILM_DIR  ${UTILS_ILM_DIR}/${TARGET_ARCHITECTURE})

add_subdirectory(ilitp)
add_subdirectory(ilmtp)
add_subdirectory(symtab)
add_subdirectory(machar)
add_subdirectory(upper)
add_subdirectory(n2rst)

# Local Variables:
# mode: cmake
# End:
