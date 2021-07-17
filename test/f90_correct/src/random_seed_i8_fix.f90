!** Copyright (c) 2019, Arm Ltd.  All rights reserved.

!** Licensed under the Apache License, Version 2.0 (the "License");
!** you may not use this file except in compliance with the License.
!** You may obtain a copy of the License at
!**
!**     http://www.apache.org/licenses/LICENSE-2.0
!**
!** Unless required by applicable law or agreed to in writing, software
!** distributed under the License is distributed on an "AS IS" BASIS,
!** WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
!** See the License for the specific language governing permissions and
!** limitations under the License.

!* Test random_seed fix
program test
  integer, parameter :: num = 1
  integer rslts(num), expect(num)
  data expect / 1 /

  call test_with_sized_seed_array()
  call test_with_large_seed_array()
  call test_with_small_seed_array()

  rslts(1) = 1
  call check(rslts, expect, num)
contains
subroutine test_with_sized_seed_array()
  integer :: my_seed_sz
  integer, allocatable :: my_seed_arr(:)
  real :: my_rand

  call random_seed(size=my_seed_sz)

  allocate(my_seed_arr(my_seed_sz))
  my_seed_arr = 0
  my_seed_arr(1) = Z'800000000000000'

  call random_seed(put=my_seed_arr)
  call random_number(my_rand)

  deallocate(my_seed_arr)
end subroutine

subroutine test_with_small_seed_array()
  integer, parameter :: my_seed_sz=8
  integer :: my_seed_arr(my_seed_sz)
  real :: my_rand

  my_seed_arr = 0
  my_seed_arr(1) = Z'800000000000000'

  call random_seed(put=my_seed_arr)
  call random_number(my_rand)
end subroutine

subroutine test_with_large_seed_array()
  integer, parameter :: my_seed_sz=51
  integer :: my_seed_arr(my_seed_sz)
  real :: my_rand

  my_seed_arr = 0
  my_seed_arr(1) = Z'800000000000000'

  call random_seed(put=my_seed_arr)
  call random_number(my_rand)
end subroutine
end program
