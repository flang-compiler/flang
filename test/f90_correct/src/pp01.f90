!*** Copyright (c) 1998, NVIDIA CORPORATION.  All rights reserved.
!***
!*** Licensed under the Apache License, Version 2.0 (the "License");
!*** you may not use this file except in compliance with the License.
!*** You may obtain a copy of the License at
!***
!***     http://www.apache.org/licenses/LICENSE-2.0
!***
!*** Unless required by applicable law or agreed to in writing, software
!*** distributed under the License is distributed on an "AS IS" BASIS,
!*** WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
!*** See the License for the specific language governing permissions and
!*** limitations under the License.
!
! Test pointer assignments and associated function
!

	integer,parameter::n=15
	integer result(n),expect(n)
	integer,pointer :: ap,bp,cp,dp
	integer,target  :: a,b
	a = 1
	b = 1
	expect(:) = 1
	result(:) = 1

	ap => a
	bp => b
	if( .not.associated(ap) ) then
	    result(1) = 0
	    print *,'ap should be associated'
	endif
	if( .not.associated(ap,a) ) then
	    result(2) = 0
	    print *,'ap should be associated with a'
	endif
	if( .not.associated(bp) ) then
	    result(3) = 0
	    print *,'bp should be associated'
	endif
	if( .not.associated(bp,b) ) then
	    result(4) = 0
	    print *,'bp should be associated with b'
	endif
	if( associated(bp,a) ) then
	    result(5) = 0
	    print *,'bp should not be associated with a'
	endif
	if( associated(ap,b) ) then
	    result(6) = 0
	    print *,'ap should not be associated with b'
	endif
	if( associated(ap,bp) ) then
	    result(7) = 0
	    print *,'ap should not be associated with bp'
	endif

	cp => a
	if( .not.associated(cp,a) ) then
	    result(8) = 0
	    print *,'cp should be associated with a'
	endif
	if( associated(cp,b) ) then
	    result(9) = 0
	    print *,'cp should not be associated with b'
	endif
	if( .not.associated(cp,ap) ) then
	    result(10) = 0
	    print *,'cp should be associated with ap'
	endif
	if( associated(cp,bp) ) then
	    result(11) = 0
	    print *,'cp should not be associated with bp'
	endif

	nullify(dp)
	nullify(cp)
	if( associated(cp) ) then
	    result(12) = 0
	    print *,'cp should not be associated'
	endif
	if( associated(dp) ) then
	    result(13) = 0
	    print *,'dp should not be associated'
	endif
	if( associated(dp,a) ) then
	    result(14) = 0
	    print *,'dp should not be associated with a'
	endif
	if( associated(dp,cp) ) then
	    result(15) = 0
	    print *,'dp should not be associated with cp'
	endif
	call check(result,expect,n)
        end
