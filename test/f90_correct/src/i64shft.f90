! Copyright (c) 2017, NVIDIA CORPORATION.  All rights reserved.
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

program i64shft
  integer(8) :: res(4), expect(4), x, y

  x = 1
  y = 32
  res = do_shfts(x, y)

  expect = 4294967296

  call check(res, expect, 4)

  contains

   function do_shfts(x, y) result(r)
     integer(8) :: x, y, r(4)
     integer(8) :: z = ishft(1, 32) ! compile-time

     r(1) = z
     r(2) = ishft(z, 1)
     r(2) = ishft(r(2), -1)
     r(3) = ishft(x,32)
     r(4) = ishft(x,y)
   end function
end program
