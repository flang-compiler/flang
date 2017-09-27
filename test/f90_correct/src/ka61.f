** Copyright (c) 1989, NVIDIA CORPORATION.  All rights reserved.
**
** Licensed under the Apache License, Version 2.0 (the "License");
** you may not use this file except in compliance with the License.
** You may obtain a copy of the License at
**
**     http://www.apache.org/licenses/LICENSE-2.0
**
** Unless required by applicable law or agreed to in writing, software
** distributed under the License is distributed on an "AS IS" BASIS,
** WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
** See the License for the specific language governing permissions and
** limitations under the License.

* Vectorizer 'induction variable retained' test


	program ka61
	common /matrix/ iord(4),irev(8)
	common /integ/ nx,ny,nz
	integer i, j
	integer result(12), expect(12)
	data expect /
     &		5,3,2,8,			! iord
     &		0,3,2,0,1,0,0,4 /		! irev
	nx = 2
	ny = 2
	nz = 2
	call combt()
	j = 1
	do i = 1,4
	    result(j) = iord(i)
	    j = j + 1
	enddo
	do i = 1,8
	    result(j) = irev(i)
	    j = j + 1
	enddo
	call check(result, expect, 12)
	end


	subroutine combt()
	implicit double precision(a-h,o-z)
	common /matrix/ iord(4),irev(8)
	common /integ/ nx,ny,nz
	icount = 0
        nxy=nx*ny
        mxyz=nx+ny+nz
	do i=4,mxyz,2
	    k1=max0(1,i-nx-ny)
	    k2=min0(i-2,nz)
	    do iinz=k1,k2
		inz=k2+k1-iinz
		j1=max0(1,i-nz-nx)
		j2=min0(i-inz-1,ny)
		do iiny=j1,j2
		    iny=j2+j1-iiny
		    inx=i-inz-iny
		    if(inx.le.nx) then
			icount=icount+1
			iord(icount)=nxy*(inz-1)+nx*(iny-1)+inx
			irev(iord(icount))=icount
		    endif
		enddo
	    enddo
	enddo
	end
