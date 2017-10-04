!* Copyright (c) 1998, NVIDIA CORPORATION.  All rights reserved.
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

*   Parallel do - simple schedule, large # of elements, > 2 processors
	program test
	parameter (NTESTS=102)
	integer expect(NTESTS)
	common/comp/ia(1000),ib(1000)
	call fill()
	call sub(NTESTS-1)  !define elements 1-101, #102 is unchanged
!	print 99, (ia(i), i=1,102)
!99	format ((5x,'+',10(i3,',')))
	data expect /
     +  1,  2,  3,  4,  5,  6,  7,  8,  9, 10,
     + 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
     + 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
     + 31, 32, 33, 34, 35, 36, 37, 38, 39, 40,
     + 41, 42, 43, 44, 45, 46, 47, 48, 49, 50,
     + 51, 52, 53, 54, 55, 56, 57, 58, 59, 60,
     + 61, 62, 63, 64, 65, 66, 67, 68, 69, 70,
     + 71, 72, 73, 74, 75, 76, 77, 78, 79, 80,
     + 81, 82, 83, 84, 85, 86, 87, 88, 89, 90,
     + 91, 92, 93, 94, 95, 96, 97, 98, 99,100,
     +101,  0 /
	call check(ia, expect, NTESTS)
	end
	subroutine sub(n)
	common/comp/ia(1000),ib(1000)
!$omp parallel do
	do i = 1, n
	ia(i) = ib(i)
	enddo
!$omp endparalleldo
	end
	subroutine fill
	common/comp/ia(1000),ib(1000)
	do i = 1, 1000
	ib(i) = iii(i)
	enddo
	end
	integer function iii(i)
	iii = i
	end

