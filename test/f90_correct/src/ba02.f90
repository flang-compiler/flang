!* Copyright (c) 2005, NVIDIA CORPORATION.  All rights reserved.
!*
!* Licensed under the Apache License, Version 2.0 (the "License");
!* you may not use this file except in compliance with the License.
!* You may obtain a copy of the License at
!*
!*     http://www.apache.org/licenses/LICENSE-2.0
!*
!* Unless required by applicable law or agreed to in writing, software
!* distributed under the License is distributed on an "AS IS" BASIS,
!* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
!* See the License for the specific language governing permissions and
!* limitations under the License.

!   passing sections to a subroutine, test section descriptor construction

module mm
contains
 subroutine s(x)
  integer x(:,:)
  !call pgf90_show(x)
  x = x + 1
 end subroutine
end module
program p
 use mm
 integer d(10,10,10)
 integer r(30)
 integer e(30)
 data e/372,340,404,372,136,104,136,104,104,72, &
        372,340,404,372,136,104,136,104,104,72, &
         80,248,248,248,248,248,248,248,248,80/
 d = 0
 call s(d(:,:,1))
 call s(d(:,:,2))
 call s(d(:,:,3))
 call s(d(:,:,4))
 call s(d(:,1,:))
 call s(d(:,2,:))
 call s(d(:,3,:))
 call s(d(:,4,:))
 call s(d(2:9,:,1))
 call s(d(2:9,:,2))
 call s(d(2:9,:,3))
 call s(d(2:9,:,4))
 call s(d(2:9,1,:))
 call s(d(2:9,2,:))
 call s(d(2:9,3,:))
 call s(d(2:9,4,:))
 call s(d(2:9,3:8,1))
 call s(d(2:9,3:8,2))
 call s(d(2:9,3:8,3))
 call s(d(2:9,3:8,4))
 call s(d(2:9,1,3:8))
 call s(d(2:9,2,3:8))
 call s(d(2:9,3,3:8))
 call s(d(2:9,4,3:8))
 call s(d(2:9,1:9:2,1))
 call s(d(2:9,1:9:2,2))
 call s(d(2:9,1:9:2,3))
 call s(d(2:9,1:9:2,4))
 call s(d(2:9,1,1:9:2))
 call s(d(2:9,2,1:9:2))
 call s(d(2:9,3,1:9:2))
 call s(d(2:9,4,1:9:2))
 !open(unit=8,file='s.out')
 !write(8,'(10i4)') d
 !close(unit=8)
 do i = 1,10
  r(i) = sum(d(:,:,i))
 enddo 
 do i = 1,10
  r(i+10) = sum(d(:,i,:))
 enddo 
 do i = 1,10
  r(i+20) = sum(d(i,:,:))
 enddo 
 !write(*,'(10i6)') r
 call check(r,e,30)
end
