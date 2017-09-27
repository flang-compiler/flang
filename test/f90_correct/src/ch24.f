! Copyright (c) 2005, NVIDIA CORPORATION.  All rights reserved.
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
! repeat intrinsic, nested
!    bug exposed in tonto
!
	common/yyy/ yyy
	common/zzz/ zzz
	parameter(N=101+5)
	integer expect(N)
	data expect/100*45,32,43,45,43,45,32/
	integer result(N)
	character*250 yyy,zzz

	call sub('+-', 10)

	do i = 1, 101
	    result(i) = ichar(yyy(i:i))
	enddo
	result(102) = ichar(zzz(1:1))
	result(103) = ichar(zzz(2:2))
	result(104) = ichar(zzz(199:199))
	result(105) = ichar(zzz(200:200))
	result(106) = ichar(zzz(201:201))
	call check(result, expect, N);
	end
	subroutine sub(ch, n)
	character*(*) ch
	common/yyy/ yyy
	common/zzz/ zzz
	character*250 yyy,zzz
	yyy = repeat(repeat('-',n),n)
	zzz = repeat(repeat(ch,n),n)
	end
