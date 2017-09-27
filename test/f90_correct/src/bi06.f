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
! bind(c) complex function -- -O delete_stores() failure; derived from the
!    entry test, ie10.f
       complex function cp1(i, j) bind(c)
       cp1 = cmplx(i, j)
       cp1 = cp1 + (1, 1)
       end
       interface 
         complex function cp1(i, j) bind(c)
         endfunction
       endinterface
       integer(4) expect(2)
       integer(4) res(2)
       data expect/2,4/
       complex z
       z = cp1(1,3)
       res(1) = real(z)
       res(2) = aimag(z)
!       print *, z
       call check(res, expect, 2)
       end
