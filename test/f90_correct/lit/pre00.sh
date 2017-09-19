#!/bin/bash
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


# Shared lit script for each tests. Run bash commands that run tests with make.

# RUN: FLAGS=%flags TEST_SRC=%s MAKE_FILE_DIR=%S/.. bash %s | tee %t 
# RUN: cat %t | FileCheck %s

test_name=${TEST_SRC##*/}  # Strip path.
test_name=${test_name%.*}  # Strip extension.

echo "Test name: $test_name"

MAKE_FILE=$MAKE_FILE_DIR/makefile

export PATH=$PATH:$(pwd)

make -f $MAKE_FILE HOMEQA=$MAKE_FILE_DIR TEST=$test_name OPT="$FLAGS" build
make -f $MAKE_FILE HOMEQA=$MAKE_FILE_DIR TEST=$test_name OPT="$FLAGS" run 
make -f $MAKE_FILE HOMEQA=$MAKE_FILE_DIR TEST=$test_name OPT="$FLAGS" verify
# CHECK: {{[1-9][0-9]* tests PASSED. 0 tests failed}}
