!* Copyright (c) 1998, NVIDIA CORPORATION.  All rights reserved.
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
!   forall containing derived type assignments


type junk
 integer m1,m2(2),m3
end type

integer result(20), expect(20)
data expect/10,20,30,40, 20,40,60,80, 30,60,90,120, &
40,80,120,160, 50,100,150,200 /

type(junk):: j(5)

forall(i=1:5)
 j(i)%m1 = 10*i
 j(i)%m2(1) = 20*i
 j(i)%m2(2) = 30*i
 j(i)%m3 = 40*i
endforall

do i = 1,5
 result(i*4-3) = j(i)%m1
 result(i*4-2) = j(i)%m2(1)
 result(i*4-1) = j(i)%m2(2)
 result(i*4-0) = j(i)%m3
enddo

!print *,result
call check( result, expect, 20 )

end
