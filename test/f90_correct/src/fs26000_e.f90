! Copyright (c) 2017-2018, NVIDIA CORPORATION.  All rights reserved.
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

! Variation from fs26000_a (integer case)
! This is logical case.

! related to a test by Neil Carlson from Los Alamos National Laboratory
! related to Flang github issue #243

type foo
  class(*), allocatable :: val
end type
type(foo) :: x
x = foo(.true.)

select type (val => x%val)
type is (logical)
  if (.not. val) then
    print *, "FAIL 1"
  else
    print *, "PASS"
  endif
class default
  print *, "FAIL 2"
end select
end
