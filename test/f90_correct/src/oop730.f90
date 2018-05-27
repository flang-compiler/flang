! Copyright (c) 2018, NVIDIA CORPORATION.  All rights reserved.
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

! complex type bound function call

module mmm
  complex, parameter :: bits(3) = [(1.0,2.0), (-3.0,-4.0), (5.0,6.0)]
  type :: vector
    complex :: data(3) = bits
  contains
    procedure :: get_value
  end type vector

contains
  complex function get_value(vec, elem)
    class(vector) :: vec
    integer       :: elem
    get_value = vec%data(elem)
  end function get_value
end

program test
  use mmm
  type(vector) :: vec
  integer      :: elem
  complex      :: ccc(3)
  do elem = 1, 3
    ccc(elem) = vec%get_value(elem)
  end do
  print*, sum(ccc), sum(bits)
  if (sum(ccc) .eq. sum(bits)) print*, 'PASS'
  if (sum(ccc) .ne. sum(bits)) print*, 'FAIL'
end
