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


! directives.h -- contains preprocessor directives for F90 rte files

#include "mmul_dir.h"

subroutine ftn_gather_cmplx16( ta, a, lda, alpha, buffer, bufrows, bufcols )
  implicit none
#ifdef TARGET_64
  integer*8 lda
#else
  integer lda
#endif
  complex*16 :: a( lda,* ), alpha
  integer :: bufrows, bufcols
  integer :: i, j, ndx, ndxsave
  complex*16 :: buffer(bufrows * bufcols)
  integer   :: ta
  !
  ! This routine gathers the matrix into l1 chunks. The purpose is much as it
  ! is for the transpose case, and works much like transpose_real8()
  !
  !   What do the parameters mean?
  !   buffer: buffer array
  !   a: matrix to be gathered
  !   bufcols: number of rows in matrix a to gather
  !   bufrowss: number of cols in matrix a to gather
  !   lda: number of rows in matrix a
  !   Note that we don't care what the dimensions of a are. We assume that the
  !   calling function has done this correctly
  !
  
  ndx = 0
  if( ta .eq. 2 )then ! conjugate the data
     if( alpha .eq. ( 1.0, 0.0 ) ) then
        do j = 1, bufcols
           do i = 1, bufrows
              buffer( ndx + i ) = conjg(  a( i, j ) )
           enddo
           ndx = ndx + bufrows
        enddo
     else
        do j = 1, bufcols
           do i = 1, bufrows
              buffer( ndx + i ) = alpha * conjg(  a( i, j ) )
           enddo
           ndx = ndx + bufrows
        enddo
     endif
  else
     if( alpha .eq. ( 1.0, 0.0 ) ) then
        do j = 1, bufcols
           do i = 1, bufrows
              buffer( ndx + i ) = a( i, j ) 
           enddo
           ndx = ndx + bufrows
        enddo
     else
        do j = 1, bufcols
           do i = 1, bufrows
              buffer( ndx + i ) = alpha * a( i, j )
           enddo
           ndx = ndx + bufrows
        enddo
     endif
  endif
     !      write( *, * ) ( a(1, j ), j = 1, bufcols )
  !      write( *, * )( buffer( i ), i = 1, bufrows * bufcols )
  return
end subroutine ftn_gather_cmplx16
