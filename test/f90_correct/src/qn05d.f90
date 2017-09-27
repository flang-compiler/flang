!** Copyright (c) 2000, NVIDIA CORPORATION.  All rights reserved.
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
! Tests scalar mask for maxval intrinsic.


	program p
	integer a(10,10)
	logical(8) msk(10,10)
	integer afalse(10), sfalse(10)
        integer atrue(10), strue(10)
        logical(8) rslt(2)
        logical(8) expect(2)
        data expect /.true.,.true./

        do j=1,10	
	do i=1,10
	a(i,j) = i*2
	enddo
	enddo

	msk = .false.

!	do i=1,5
!	msk(i,1) = .true.
!	enddo
		
	sfalse = maxval(a,dim=1,mask=.false.)
	afalse = maxval(a,dim=1,mask=msk)
	strue = maxval(a,dim=1,mask=.true.)
        msk = .true.
        atrue = maxval(a,dim=1,mask=msk)
        rslt(1) = all(sfalse .eq. afalse)
        rslt(2) = all(atrue .eq. strue)
        call check(rslt,expect,2)
	end
