! Copyright (c) 1999, NVIDIA CORPORATION.  All rights reserved.
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
! test pointer, allocatable allocated with zero, negative size

	real,dimension(:),pointer::a
	real,dimension(:,:),pointer::b
	real,dimension(:),allocatable::c

	integer,dimension(9) :: expect,result
	data expect/1,1,1,0,0,0,0,0,0/

	n = 1
	allocate(a(n))
	allocate(b(n,n))
	allocate(c(n))
	!print 5,n
	!print 10,'a',lbound(a,1),ubound(a,1),size(a)
	!print 20,'b',lbound(b,1),ubound(b,1),lbound(b,2),ubound(b,2),size(b)
	!print 10,'c',lbound(c,1),ubound(c,1),size(c)
	result(1) = size(a)
	result(2) = size(b)
	result(3) = size(c)
	deallocate (c)     !4/16/2000 - can't allocate if already allocated.
	n = 0
	allocate(a(n))
	allocate(b(n,n))
	allocate(c(n))
	!print 5,n
	!print 10,'a',lbound(a,1),ubound(a,1),size(a)
	!print 20,'b',lbound(b,1),ubound(b,1),lbound(b,2),ubound(b,2),size(b)
	!print 10,'c',lbound(c,1),ubound(c,1),size(c)
	result(4) = size(a)
	result(5) = size(b)
	result(6) = size(c)
	deallocate (c)     !4/16/2000 - can't allocate if already allocated.
	n = -1
	allocate(a(n))
	allocate(b(n,n))
	allocate(c(n))
	!print 5,n
	!print 10,'a',lbound(a,1),ubound(a,1),size(a)
	!print 20,'b',lbound(b,1),ubound(b,1),lbound(b,2),ubound(b,2),size(b)
	!print 10,'c',lbound(c,1),ubound(c,1),size(c)
	result(7) = size(a)
	result(8) = size(b)
	result(9) = size(c)
	deallocate (c)     !4/16/2000 - can't allocate if already allocated.
	call check(result,expect,size(expect))
5	format( 'n=',i2 )
10	format( a,'(',i2,':',i2,'), size=',i2)
20	format( a,'(',i2,':',i2,',',i2,':',i2,', size=)',i3)
	end
