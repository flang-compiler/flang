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

program p

  parameter(NbrTests=18)

  complex*8, dimension(4,3) :: arr1
  complex*8, dimension(4) :: arr2
  complex*8, dimension(3) :: arr3


  data arr1 /(0,1),(1,2),(2,3),(3,4), &
             (4,3),(5,4),(6,5),(7,6), &
             (8,8),(9,9),(10,10),(11,11)/
  data arr2 /(0,4),(1,3),(2,2),(3,1)/
  data arr3 /(0,2),(1,3),(2,1)/

  complex*8 :: expect(NbrTests)
  complex*8 :: results(NbrTests)

  data expect / &
   !test 1,6
     (-6.0,30.0), (-2.0,82.0), (-28.0,152.0), &
   !test 7,12
     (-2.0,30.0), (10.0,66.0), (4.0,120.0), &
   !test 13,18
     (-11.0,15.0), (-17.0,57.0), (-50.0,108.0), &
   !test 19,24
     (-2.0,18.0), (10.0,54.0), (4.0,108.0), &
   !test 25,30
     (-2.0,30.0), (10.0,66.0), (0.0,0.0), &
   !test 43,36
     (0.0,0.0), (-2.0,82.0), (-28.0,152.0)/


  results = -1

  ! tests 1-6
  arr3=0
  arr3 = matmul(transpose(arr1),arr2)
  call assign_result(1,3,arr3,results)
  !print *,"test 1,6"
  !print *,arr3
  
  ! tests 7-12
  arr3=0
  arr3 = matmul(transpose(arr1(2:4,:)),arr2(2:4))
  call assign_result(4,6,arr3,results)
  !print *,"test 7,12"
  !print *,arr3
  
  ! tests 13-18
  arr3=0
  arr3 = matmul(transpose(arr1(1:3,:)),arr2(1:3))
  call assign_result(7,9,arr3,results)
  !print *,"test 13,18"
  !print *,arr3
  
  !tests 19-24
  arr3=0
  arr3 = matmul(transpose(arr1(1:3,:)),arr2(2:4))
  call assign_result(10,12,arr3,results)
  !print *,"test 19,24"
  !print *,arr3
  
  !tests 25-30
  arr3=0
  arr3(1:2) = matmul(transpose(arr1(2:4,1:2)),arr2(2:4))
  call assign_result(13,15,arr3,results)
  !print *,"test 25,30"
  !print *,arr3
  
  !tests 31-36
  arr3=0
  arr3(2:3) = matmul(transpose(arr1(:,2:3)),arr2)
  call assign_result(16,18,arr3,results)
  !print *,"test 31,36"
  !print *,arr3
  
  call check(results, expect, NbrTests*2)

end program

subroutine assign_result(s_idx, e_idx , arr, rslt)
  complex*8, dimension(1:e_idx-s_idx+1) :: arr
  complex*8, dimension(e_idx) :: rslt
  integer:: s_idx, e_idx

  rslt(s_idx:e_idx) = arr

end subroutine

