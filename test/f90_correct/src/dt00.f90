!* Copyright (c) 1997, NVIDIA CORPORATION.  All rights reserved.
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
!
!   Derived types

program p
type test
 integer,dimension(:,:),pointer:: mem
end type
integer results(9), expect(9)
data expect /4,5,6,7,99,9,10,99,12/

type(test)::a

allocate(a%mem(1:3,1:3))

do i = 1,3
 do j = 1,3
  a%mem(i,j) = i+j*3
 enddo
enddo

where(a%mem(1,:).gt.5) 
 a%mem(2,:) = 99
endwhere

do i = 1,3
 do j = 1,3
  k = i+(j-1)*3
  results(k) = a%mem(i,j)
 enddo
enddo
call check( results, expect, 9)
end
