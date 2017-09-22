
!** Copyright (c) 1989, NVIDIA CORPORATION.  All rights reserved.
!**
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

!* Tests for runtime library MATMUL routines

module typ
  type pippo
    real :: h(3,3)
  end type
end module

program p
  use typ

  parameter(NbrTests=9)
  
  type(pippo):: a, b, c

  REAL :: expect(NbrTests) 
  REAL :: results(NbrTests)
  
  data expect /  &
  ! test 1-48
  3,3,3,3,3,3,3,3,3/
  
  !test 1-9
   a%h = 1.0
   b%h = 1.0
   c%h = matmul( a%h, b%h )
   call assign_result(1,9,c%h,results)
  ! print *,c%h
  
  call check(results, expect, NbrTests)
end program

subroutine assign_result(s_idx, e_idx , arr, rslt)
  REAL, dimension(1:e_idx-s_idx+1) :: arr
  REAL, dimension(e_idx) :: rslt
  integer:: s_idx, e_idx

  rslt(s_idx:e_idx) = arr

end subroutine
