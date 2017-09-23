! Copyright (c) 1990-2017, NVIDIA CORPORATION.  All rights reserved.
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

! example generates error 'deferred shape array must have
! POINTER attribute in a derived type'

program test_pgi
implicit none

integer, dimension (3), parameter :: &
tgc = (/100,101,102/)

type test
integer :: id
end type test

type testData
type(test),dimension(tgc(1):tgc(1)+size(tgc)-2) :: testlist
end type testData

type(testData) :: list

integer*4, dimension(3) :: results
integer*4, dimension(3) :: expect = (/ 2, 100, 101 /)

results(1) = size(list%testlist)
results(2) = lbound(list%testlist, 1)
results(3) = ubound(list%testlist, 1)

call check(results, expect, 3)

end

