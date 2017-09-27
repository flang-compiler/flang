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
! This example is slightly modified from the Preprocessor chapter in the
! C99 spec: Example 6 in section 6.10.3.4:
! Test legal redefinition rules 
!
! ** Currently we pass comments (either macro-expanded or not) to the lexer...
! ** this will generate two warnings regarding macro-redefinition.
!
#define OBJ_LIKE (1-1)
#define OBJ_LIKE (1-1) !Comment
#define FUNC_LIKE(x) (x)
#define FUNC_LIKE(x) (x) !Comment
program p
    ! This test will only produce a warnings
    call check(.true., .true., 1)
end program
