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

logical function baseproc(this,this2)
class(base_t) :: this
class(base_t), optional :: this2

if (present(this2)) then 
    baseproc = .true.
else 
    baseproc = .false.
endif 
end function

end module

program p
USE CHECK_MOD
use mod
logical results(2)
logical expect(2)
data results /.true.,.false./
data expect /.false.,.true./
type(base_t) :: t

results(1) = t%some_proc()
results(2) = t%some_proc(t)

call check(results,expect,2)

end program 
