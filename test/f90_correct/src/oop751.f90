! Copyright (c) 2019, NVIDIA CORPORATION.  All rights reserved.
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

module mod
type, abstract :: base
end type
type, extends(base) :: t
  integer :: i
end type
end module

program p
  use mod
  logical rslt(3),expect(3)
  class(base), allocatable :: a(:)
  class(t), allocatable :: b(:)
  type(t) :: o

  allocate(a(10), b(5), mold=o)

  select type(p => a)
  type is (t)
  rslt(1) = .true.
  class default
  rslt(1) = .false.
  end select

  select type(p => b(1))
  type is (t)
  rslt(2) = .true.
  class default
  rslt(2) = .false.
  end select

  rslt(3) = same_type_as(a,b)

  expect = .true.
  call check(rslt,expect,3)


  end
