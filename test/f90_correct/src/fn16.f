!
! Copyright (c) 2017, NVIDIA CORPORATION.  All rights reserved.
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
! From pwsfC:
! array-valued functions whose size depends on the size of a
! an argument whose correspoinding actual is a module allocatable
! array.
	module fn16
	real, allocatable :: zzz(:,:)
	contains
	    function ff1(xx)
	    real :: xx(:,:)
	    real :: ff1(size(xx))
	    ff1 = 11
	    endfunction
	    function ff2(xx)
	    real :: xx(:,:)
	    real :: ff2(size(xx,1))
	    ff2 = 13
	    endfunction
	    function ff3(xx,i)
	    real :: xx(:,:)
	    real :: ff3(size(xx,i))
	    ff3 = 17
	    endfunction
	endmodule
	subroutine sub(ii)
	use fn16
	integer :: result(3)
	integer :: expect(3)=(/66,26,51/)
	allocate(zzz(2,3))
	result(1) = sum(ff1(zzz))
	result(2) = sum(ff2(zzz))
	result(3) = sum(ff3(zzz,ii))
	call check(result, expect, 3)
	end
	call sub(2)
	end
