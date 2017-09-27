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
logical result
contains
procedure, pass   :: baseproc_pass => baseproc
procedure, nopass :: baseproc_nopass => baseproc
generic           :: some_proc => baseproc_pass, baseproc_nopass
end type

type, extends(base_t) :: ext_t
end type

contains

subroutine baseproc(this)
class(base_t) :: this
select type(this)
type is(base_t)
this%result = .true.
type is (ext_t)
this%result = .false.
class default
stop 'baseproc: unexepected type for this'
end select
end subroutine

end module

program p
USE CHECK_MOD
use mod
logical results(2)
logical expect(2)
data results /.false.,.true./
data expect /.true.,.false./
type(base_t) :: t
type(ext_t) :: t2

  t%result = .false.
  t2%result = .true. 
  call t%some_proc()
  results(1) = t%result
  call t%some_proc(t2)
  results(2) = t2%result

  call check(results,expect,2)

end program 
