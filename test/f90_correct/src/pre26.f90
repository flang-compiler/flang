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
! This tests __VA_ARGS__ replacement in macros
!
#define PR(...) print *, "Test:", __VA_ARGS__
#define CPY(...) __VA_ARGS__
program p
    integer res(4), expect(4);
    data res / CPY(1,2,3),4 /
    data expect / 1,2,3,4 /
    PR("This is a string with values", 1,2,3)
    call check(res, expect, 4)
end program
