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
! error in nag95 test ch7egs
!
! IPA propagation of array bounds from program to 'b'
! where the bound for 'b' was used in the bound for an
! automatic array c1 or c2 here, c1/c2 bounds didn't
! get handled properly
!
	module m

	integer res(4)

	contains
	 subroutine s(a,b)
	  integer a(1:), b(10:)
	  integer c1(ubound(b,1))
	  integer c2(size(b,1))
	  integer n,m
	  n = sum(a)
	  m = sum(b(10:))
	  !print *,n,'=sum(a)',m,'=sum(b)'
	  !print *,lbound(a,1),':',ubound(a,1)
	  !print *,lbound(b,1),':',ubound(b,1)
	  res(1) = n
	  res(2) = m
	  c1 = 1
	  c2 = 1
	  n = sum(c1)
	  m = sum(c2)
	  !print *,n,'=sum(c1)',m,'=sum(c1)'
	  !print *,lbound(c1,1),':',ubound(c1,1)
	  !print *,lbound(c2,1),':',ubound(c2,1)
	  res(3) = n
	  res(4) = m
	 end subroutine
	end module

	program p

	 use m

	 integer a(10),b(10)
	 integer exp(4)
	 data exp/10,20,19,10/

	 a = 1
	 b = 2

	 call s(a,b)

	 call check(res,exp,4)
	end
