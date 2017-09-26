!** Copyright (c) 2001, NVIDIA CORPORATION.  All rights reserved.
!**
!** Licensed under the Apache License, Version 2.0 (the "License");
!** you may not use this file except in compliance with the License.
!** You may obtain a copy of the License at
!**
!**     http://www.apache.org/licenses/LICENSE-2.0
!**
!** Unless required by applicable law or agreed to in writing, software
!** distributed under the License is distributed on an "AS IS" BASIS,
!** WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
!** See the License for the specific language governing permissions and
!** limitations under the License.
!
! correctly handle subroutines that appearly entirely in an include file
! problem was ENLAB was only put out when nonzero STD_LINENO is seen,
! but STD_LINNO is not set for include file lines

	subroutine s(a)
	a = 1
	end

	subroutine t(a)
	if(a .gt. 0 ) goto 50
50	continue
	return
90	continue
	a = 2
!
! problem is caused by 'write', which sets STD_LINENO anyway,
! so ENLAB is put out for unreachable statement
!
	write(*,*)a
	return
100	continue
	return
	end
