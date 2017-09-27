!*** Copyright (c) 1998, NVIDIA CORPORATION.  All rights reserved.
!***
!*** Licensed under the Apache License, Version 2.0 (the "License");
!*** you may not use this file except in compliance with the License.
!*** You may obtain a copy of the License at
!***
!***     http://www.apache.org/licenses/LICENSE-2.0
!***
!*** Unless required by applicable law or agreed to in writing, software
!*** distributed under the License is distributed on an "AS IS" BASIS,
!*** WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
!*** See the License for the specific language governing permissions and
!*** limitations under the License.

! more derived type parameters in modules
	module md
	  type tscope
	     integer :: scope
	     real :: junk
	  end type
	  type (tscope), parameter :: local = tscope(2,3.333)
	  type (tscope), parameter :: global = tscope(1,4.444)
	end module
	subroutine sub(a,x)
	  use md
	  real,parameter::l = local%junk
	  real,parameter::g = global%junk
	  integer,parameter::mm = local%scope
	  real array(local%scope)
	  real a
	  integer x
	  select case(x)
	  case(local%scope)
	    a = l
	  case(global%scope)
	    a = g
	  case default
	    a = 0.0
	  end select
	end subroutine

	program p
	  use md
	  type(tscope)::m
	  parameter(n=3)
	  real result(n)
	  real expect(n)
	  data expect/3.333,4.444,0.000/
	  call sub(result(1),local%scope)
	  call sub(result(2),global%scope)
	  call sub(result(3),0)
	  call check(result, expect, n)
	end
