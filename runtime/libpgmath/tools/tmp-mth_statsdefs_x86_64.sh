#!/bin/bash

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

# First arg is path to source, ${CMAKE_CURRENT_SOURCE_DIR} in cmake
# called from the directories that contains the source files
# Second arg is the name of the output file created

awk \
		'/^MTH_DISPATCH_FUNC/ { \
			f = $1; \
			sub("^MTH_DISPATCH_FUNC\\(", "", f); \
			sub("\\).*", "", f); next; \
		} \
		/^[[:space:]]*_MTH_I_STATS_INC/ { \
			split($0, s, "[(,)]"); \
			print "DO_MTH_DISPATCH_FUNC(" f ", " s[2] \
				", " s[3] ", ", s[4] ")"; f=""; \
		}' \
	$1/mth_128defs.c \
	$1/mth_256defs.c \
	$1/mth_512defs.c \
	> $2
