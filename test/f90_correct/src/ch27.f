!
! Copyright (c) 2014, NVIDIA CORPORATION.  All rights reserved.
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

! allocatable character array with deferred length

	program allocdeferredlen
	call sub
	end
	subroutine sub
        integer, parameter :: NT = 6
        integer, parameter :: N = 21
	integer*4 result(NT), expect(NT)
	data expect/48,-21,21,1,1,43/
        character(len=:), allocatable :: FileName(:)

        allocate ( character(len=48) :: FileName(-N:N) )
        FileName(:) = (/ ( char(64+k), k = -N, N ) /)
        result(1) = len(FileName)
        result(2) = lbound(FileName,1)
        result(3) = ubound(FileName,1)
	deallocate(FileName)

! ========   note the intersting allocatable assignment semantics here!!!
        allocate ( character(len=48) :: FileName(-N:N) )
        FileName = (/ ( char(64+k), k = -N, N ) /)
        result(4) = len(FileName)
        result(5) = lbound(FileName,1)
        result(6) = ubound(FileName,1)

!	write(0, '(6i4)') expect, result
	call check(result, expect, NT)

        end
