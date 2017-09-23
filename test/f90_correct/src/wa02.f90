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
!	Test that array constructors are allowed as 'put' argument to
!	random_seed

	program pp
	integer n
	integer, allocatable :: s(:)
	integer result,expect
	data expect/1/

	call random_seed(size=n)

	allocate (s(1:n))
	s = (/ ( i, i=1,n ) /)
	call random_seed(put=s)

	call random_seed(put=(/ ( i, i=1,n ) /) )

	result = 1
	call check(result,expect,1)
	end
