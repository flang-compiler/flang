!
! Copyright (c) 2015-2017, NVIDIA CORPORATION.  All rights reserved.
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
!           c_sizeof() can be applied to elements of a pointer, allocatable, 
!           assumed-shape, and assumed-size array
module bi04
   use iso_c_binding
   integer result(4)
   integer expect(4)
   data expect/4,8,4,8/
   contains
   subroutine sub(aa, pp, yy, zz)
     real, allocatable :: aa(:)
     real, pointer :: pp(:)
     real(8)       :: yy(:)
     real(8)       :: zz(*)
     result(1) = c_sizeof(pp(1))
     result(2) = c_sizeof(yy(1))
     result(3) = c_sizeof(aa(1))
     result(4) = c_sizeof(zz(1))
!     print 99, result
!     print 99, expect
!  99 format(6i4)
     call check(result, expect, 4)
    endsubroutine
end
use bi04
real, allocatable :: aa(:)
real, pointer :: pp(:)
real(8)       :: yy(7)
allocate(aa(0:4))
allocate(pp(3:9))
call sub(aa, pp, yy, yy)
end
