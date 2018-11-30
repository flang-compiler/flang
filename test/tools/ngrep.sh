#!/bin/sh
#
# Copyright (c) 2016-2018, NVIDIA CORPORATION.  All rights reserved.
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

grep -i "$1" $2 > $2.grep
if test $? = 1 ; then
    if [ $3 ]; then
      echo "File $2 does not contain the string that contains the failure"
    else
      echo "File $2 does not contain the string $1."
    fi
    echo " Test PASSES" 
    echo " PASS "
else
    echo "File $2 contains the string $1."
    echo " Test FAILS" 
fi
