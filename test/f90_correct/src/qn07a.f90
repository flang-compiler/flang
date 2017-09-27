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
! Tests scalar mask for product intrinsic.


	program p

	integer stuff(3,3)
	logical(1) msk1(3,3),msk2(3,3)
	logical expect(2)
	logical rslt(2)
	data expect /.true.,.true./
	integer atrue(3), afalse(3)
	integer sfalse(3), strue(3)

	do i=1,3
	do j=1,3
	stuff(i,j) = i
	enddo
	enddo

	msk1 = .true.
	msk2 = .false.

	atrue = product(stuff,dim=1,mask=msk1)
	strue = product(stuff,dim=1,mask=.true.)
	afalse = product(stuff,dim=1,mask=msk2)
	sfalse = product(stuff,dim=1,mask=.false.)
	rslt(1) = all(atrue .eq. strue)
	rslt(2) = all(afalse .eq. sfalse)
	call check(rslt,expect,2)

	end

