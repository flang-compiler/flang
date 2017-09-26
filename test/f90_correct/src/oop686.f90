!
! Copyright (c) 2016, NVIDIA CORPORATION.  All rights reserved.
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
program bug
  type :: otype
    integer member
  end type otype
  type(otype) :: obj
  obj%member = 1
  call subr(obj)
  call check(obj%member, 2, 1)

contains
  subroutine subr(obj)
    class(otype), intent(inout) :: obj
    type(otype) :: new
    new = otype(obj%member + 1)
    select type(obj)
      type is (otype)
        ! pgf90 incorrectly complains here about assignment
        ! to a polymorphic object, despite being in the range
        ! of a type is clause in a select type construct.
        ! (F2008 8.1.5.2 para 5)
        obj = new
    end select
  end subroutine subr
end program bug
