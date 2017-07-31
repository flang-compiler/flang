!
! Copyright (c) 2017, NVIDIA CORPORATION.  All rights reserved.
!
! Licensed under the Apache License, Version 2.0 (the "License");
! you may not use this file except in compliance with the License.
! You may obtain a copy of the License at
!
!     http://www.apache.org/licenses/LICENSE-2.0
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.
!

! RUN: %flang -c %s
! RUN: %flang -c -ffree-form %s
! RUN: %flang -c -fno-fixed-form %s
! RUN: %flang -c -Mfree %s
! RUN: %flang -c -Mfreeform %s
! RUN: not %flang -c -ffixed-form %s 2>&1 | FileCheck %s
! RUN: not %flang -c -fno-free-form %s 2>&1 | FileCheck %s
! RUN: not %flang -c -Mfixed %s 2>&1 | FileCheck %s
! RUN: not %flang -c -Mnofree %s 2>&1 | FileCheck %s
! RUN: not %flang -c -Mnofreeform %s 2>&1 | FileCheck %s

program f ! CHECK: Label field of continuation line is not blank
end program ! CHECK: Label field of continuation line is not blank
