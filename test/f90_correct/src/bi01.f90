!
! Copyright (c) 2015, NVIDIA CORPORATION.  All rights reserved.
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

    module test

       integer N
       parameter (  N = 28)
       integer ND
       parameter (  ND= 4 )

      interface
	subroutine plainc (a,b,c) bind(c, name = 'newc')
	integer a,b
	real c
	end  subroutine plainc 
	
	subroutine intfunc (a,b,c,d,e,f,g,h,i) bind(c)
        integer, value :: a,b,c,d,e,f,g,h
        integer *8, value :: i
        end subroutine intfunc

	subroutine logfunc (a,b,c,d,e,f,g,h) bind(c)
        integer  , value :: a,b,c,d
        logical , value :: e,f,g,h
        end subroutine logfunc

	function realfunc (a,b,c,d,e,f,g,h,i) result(bind) bind(c)
        real, value :: a,b,c,e,f,g,h
        real *8, value :: i,d
	integer bind
        end function realfunc


	subroutine check (a,b,c) bind (c)
	integer a(N), b(N), c
        end subroutine check

      end interface



   common /result/ a_array
   integer a_array(N)
   BIND (c) ::/result/

   common /expect/ e_array
   integer e_array(N)
   BIND (c) ::/expect/


   common /d_expect/ d_array
   real *8 d_array(ND)
   BIND (c) ::/d_expect/

   common /d_result/ dr_array
   real *8 dr_array(ND)
   BIND (c) ::/d_result/

   commont

   end module

	use test
	logical ll
	real*8 tt
	real bind

      integer*8 kko
      kko = 45
	ll = .FALSE.
	tt = 50.0

       call plainc (1,2,4.0)
       call intfunc(3,4,5,6,10,20,30,40,kko)
       call logfunc(7,8,9,10,.TRUE., .FALSE., .TRUE. , .FALSE. )
       a_array(25) = realfunc(2.0, 3.0, 4.0, tt,3.0,3.0,6.0,30.03E04,400.004D01)
       call check(a_array, e_array , N)
       call checkd(dr_array, d_array, ND);
      end

