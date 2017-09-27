! Copyright (c) 2011, NVIDIA CORPORATION.  All rights reserved.
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

module mod
type base_t
contains
procedure :: some_proc => baseproc
end type

contains

subroutine baseproc(this,this2,rslt)
class(base_t) :: this
class(base_t), optional :: this2
logical, intent(out) :: rslt

if (present(this2)) then 
    rslt = .true.
else 
    rslt = .false.
endif 
end subroutine

end module

program p
USE CHECK_MOD
use mod
logical results(2)
logical expect(2)
data results /.true.,.false./
data expect /.false.,.true./
type(base_t) :: t

call t%some_proc(rslt=results(1))
call t%some_proc(t,rslt=results(2))

call check(results,expect,2)

end program 
