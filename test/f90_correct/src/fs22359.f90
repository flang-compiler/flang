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

integer, PARAMETER :: AAA(3:4) = (/ -325, 400/)
integer , PARAMETER :: YYY(3:3) = (/ (abs((AAA(i))), i = 3,3) /)
integer , PARAMETER :: ZZZ = abs(aaa(3)) !!! works
integer pass

pass = 1;
if (zzz .eq. 325) then
  do i = 3, 3
    if (yyy(i) .ne. 325) then
      pass = 0
    end if
  end do
else
  pass = 0
endif

if (pass.eq.0) then
  print *, "FAIL"
else
  print *, "PASS"
endif

end
