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

! array parameters in modules
	module md
	integer,dimension(3),parameter:: siz=(/ 10,20,30 /)
	end module
	program p
	use md
	integer,dimension(siz(1))::a
	integer,dimension(siz(2))::b
	integer,dimension(siz(3))::c
	parameter(n=6)
	integer result(n)
	integer expect(n)
	data expect/1,10,1,20,1,30/
	result(1) = lbound(a,1)
	result(2) = ubound(a,1)
	result(3) = lbound(b,1)
	result(4) = ubound(b,1)
	result(5) = lbound(c,1)
	result(6) = ubound(c,1)
	call check(result, expect, n)
	end
