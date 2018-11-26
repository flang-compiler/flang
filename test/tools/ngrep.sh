#!/bin/sh
#
#     Copyright (c) 2018, NVIDIA CORPORATION.  All rights reserved.
#
# NVIDIA CORPORATION and its licensors retain all intellectual property
# and proprietary rights in and to this software, related documentation
# and any modifications thereto.  Any use, reproduction, disclosure or
# distribution of this software and related documentation without an express
# license agreement from NVIDIA CORPORATION is strictly prohibited.
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
