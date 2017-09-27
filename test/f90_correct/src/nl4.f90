!
! Copyright (c) 2015, NVIDIA CORPORATION.  All rights reserved.
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

! automatic arrays in namelist

SUBROUTINE S1(I, yy, output) 
integer :: yy(i), zz(I), aa(I), ii
NAMELIST /NLIST/ yy, zz, ii
character*40 output(8)
ii    = 5
zz(1) = 3
zz(2) = 4
!write(6     ,NML=NLIST)
write(output,NML=NLIST)
END SUBROUTINE S1

program test
integer aa(3)
data aa/1,2,-99/
character*40 output(8)
integer yy(2), zz(2), ii
NAMELIST /NLIST/ yy, zz, ii
integer result(5)
integer expect(5)
data expect/5,1,2,3,4/
call S1(2,aa,output)
!write(6, '(a)') output
read(output, nlist)
!write(6, '(6i4)') yy, zz, ii

result(1) = ii
result(2:3) = yy
result(4:5) = zz
call check(result, expect, 5)
end
