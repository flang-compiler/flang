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
procedure, pass(this) :: baseproc_pass => baseproc
procedure, nopass :: baseproc_nopass => baseproc
generic           :: some_proc => baseproc_pass, baseproc_nopass
end type

type, extends(base_t) :: ext_t
end type

contains

subroutine baseproc(v,this)
class(base_t) :: this
logical v
select type(this)
type is(base_t)
this%result = v
type is (ext_t)
this%result = .not. v
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
data results /.false.,.false./
data expect /.true.,.true./
type(base_t) :: t
type(ext_t) :: t2

  t%result = .false.
  t2%result = .true. 
  call t%some_proc(.true.)
  results(1) = t%result
  call t%some_proc(.false.,t2)
  results(2) = t2%result

  call check(results,expect,2)

end program 
