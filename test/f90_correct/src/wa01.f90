!* Copyright (c) 2000, NVIDIA CORPORATION.  All rights reserved.
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
!	check that index variables used in implied DO in array
!	constructor do not change variable of same name (i below)
!	but that those in implied DO in IO do change variable (j below)
	program pp
	integer i,j
	integer a(10)
	integer result(2),expect(2)
	data expect/1,11/
	i=1
	j = 2
	a=(/(i*2,i=1,10)/)
	write(*,*) (a(j),j=1,10)
	result(1) = i
	result(2) = j
	call check(result,expect,2)
	end
