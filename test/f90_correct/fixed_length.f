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

       ! RUN: %flang -c -Mextend %s
       ! RUN: %flang -c -ffixed-line-length-132 %s
       ! RUN: not %flang -c %s 2>&1 | FileCheck %s
       ! RUN: not %flang -c -ffixed-line-length-72 %s 2>&1 | FileCheck %s
       PRINT *, "1234567891234567891234567891234567891234567891234567891234567891234567891234567891" ! CHECK: Unmatched quote
       END

