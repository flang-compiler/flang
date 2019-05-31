! Copyright (c) 1990-2012, NVIDIA CORPORATION.  All rights reserved.
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
!  Test of issue #720
!
logical :: failed=.FALSE.
type, abstract :: json_value
end type

type, extends(json_value) :: json_string
  character(:), allocatable :: value
end type

class(json_value), allocatable :: a

allocate(a, source=json_string('foo'))  ! <== SEG FAULT HERE

select type(a)
type is (json_string)
  if (len(a%value) /= 3) failed=.TRUE.
  if (a%value /= 'foo') failed=.TRUE.
class default
  failed=.TRUE.
end select

if (failed) then
    call check(0,1,1)
else
    call check(1,1,1)
end if

end
