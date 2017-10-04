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
! Test copyprivate for entire common blocks, member of common blocks,
! regular variables, and a pointer to a common block value.

integer :: X=0, Y=0, Z=0
COMMON /XYV/ X,Y,V
COMMON /ZED/ Z
integer, save :: A=0, B=0
integer, pointer :: P
integer, target :: V
integer, dimension(6) :: results
integer, dimension(6) :: expected = (/101,201,301,401,501,601/)

!$OMP THREADPRIVATE (A, /XYV/, /ZED/, P)
!$OMP PARALLEL PRIVATE(B) NUM_THREADS(8)
    A = A + 1 ! These variables are now 1 after 
    B = 1
    X = X + 1
    Y = Y + 1
    Z = Z + 1
    V = V + 1
!$OMP SINGLE
    A = A + 100
    B = B + 200
    X = X + 300
    Y = Y + 400
    Z = Z + 500
    V = V + 600
    P => V
!$OMP END SINGLE COPYPRIVATE (A, /XYV/, Z, B, P)
    results(1) = A
    results(2) = B
    results(3) = X
    results(4) = Y
    results(5) = Z
    results(6) = P
    call check(results, expected, 6)
!$OMP END PARALLEL
END 
