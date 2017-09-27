!*
! Copyright (c) 2000, NVIDIA CORPORATION.  All rights reserved.
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
!*
!     test interface sub with subroutine sub
	module interf
	  public  sub
	  interface  sub
	    module procedure  sub, subr
	  end interface
	
	  contains
	    subroutine  sub(II)
	      integer  II
	      II = 122
	    end subroutine  sub
	    subroutine  subr(rr)
	      real  rr
	      rr = 99
	    end subroutine  subr
	end module interf
	
	program  testmod
	  use  interf
	  implicit none
	  integer  II
	  integer,dimension(2) :: result,expect
	  data expect/122,99/
	  real  rr
	  ii = 0
	  rr = 0
	
	  call sub(II)
	  call sub(rr)
	  result(1) = ii
	  result(2) = rr
	  call check(result,expect,2)
	end
