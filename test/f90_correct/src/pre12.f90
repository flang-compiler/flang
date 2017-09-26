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
! This tests define and undef macros 
!
#define FOO
#undef FOO
#define BAR 42
#define BAZ 43

program p
! Test undef
#ifdef FOO
    This will cause a compiler error
#else
    print *, "Works!"
#endif

! Test defined operator using BAR and BAZ 
#if defined(BAR) && defined(BAZ) && (BAR < BAZ)
    print *, "Works!"
#elif defined(FOO)
    This will cause a compiler error
#else
    ... So will this
#endif

! Test != vs comment
# if BAR != BAZ
    print *, "Works!"
# else
    This will cause a compiler error
# endif

    call check(.true., .true., 1)
end program
