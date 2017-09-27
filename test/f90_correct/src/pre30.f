!
! Copyright (c) 2014, NVIDIA CORPORATION.  All rights reserved.
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
! This tests a macro and a fixed-form comment (both begin with 'C')
! This should be run with pgf90:
!    -Mpreprocess
!    -mp
!    -Hx,124,0x100000 (Do not perform macro expansion of comments, as
!                      that would replace 'C a comment' line below with '42 a
!                      comment'
!    -Mfixed
!
#define C 42
C a comment
      program p
C$omp FLUSH
            if (C .eq. 42) then
                call check(.true., .true., 1)
            else
                call check(.false., .true., 1)
            endif
      end
