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

!   KIND with various basic datatypes

program p
 real(4) a
 real(8) b
 complex(4) c
 complex(8) d
 integer(1) e
 integer(2) f
 integer(4) g
 integer(8) h
 integer result(8)
 integer expect(8)
 data expect/4,8,4,8,1,2,4,8/
 result(1) = kind(a)
 result(2) = kind(b)
 result(3) = kind(c)
 result(4) = kind(d)
 result(5) = kind(e)
 result(6) = kind(f)
 result(7) = kind(g)
 result(8) = kind(h)
! print *,result
 call check(result,expect,8)
end program
