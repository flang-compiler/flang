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
! This tests stringization
!
#define _TURTLES2(_msg) _msg##"And down again..."
#define _TURTLES1(_msg) _TURTLES2(_msg ## "And down...")
#define TURTLES(_msg) _TURTLES1(_msg)

program p
    logical :: res(1) = .false., expect(1) = .true.
    print *, TURTLES("All the way down!")
    if (TURTLES("All the way down!") == 'All the way down!"And down..."And down again...') then
        res(1) = .true.
    endif

    call check(res, expect, 1)
end program
