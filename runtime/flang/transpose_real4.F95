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


#include "mmul_dir.h"


subroutine ftn_transpose_real4( a, lda, alpha, buffer, bufrows, bufcols )
  implicit none
#ifdef TARGET_64
  integer*8 lda
#else
  integer lda
#endif
  integer :: bufrows, bufcols
  integer i, j, ndx, ndxsave
  real*4 :: a( lda, * ), alpha
  real*4 :: buffer(bufrows * bufcols)

  !
  !   The plan here is to copy the matrix a to the buffer, or at least a 
  !   portion of it, such that the matrix  (really a buffer) is in proper 
  !   order for successive access. Some number of columns of a will be 
  !   dispersed to buffer to minimize page faults.
  !   The calling function can manage the buffer for both L1 and L2 cache
  !   utilization. bufcols defines the number of values taken from L1 cache
  !   for each dot product. bufrows * bufcols defines how much L2 cache is 
  !   used.
  !
  !   We may want to change this to be able to handle multiple sections of L1
  !   cache usage such as giving an additional parameter, say, nbufrows
  !   which would essentially copy more of the matrix a to the buffer using
  !   an additional loop
  
  !
  !   What do the parameters mean?
  !   buffer: buffer array
  !   a: matrix to be transposed
  !   bufcols: number of rows in matrix a to transpose
  !   bufrowss: number of cols in matrix a to transpose
  !   lda: number of rows in matrix a
  !   Note that we don't care what the dimensions of a are. We assume that the
  !   calling function has done this correctly
  !
  
  ndxsave = 1
  do j = 1, bufrows
     ndx = ndxsave
     do i = 1, bufcols
        buffer( ndx ) = alpha * a( i, j )
        ndx = ndx + bufrows
     enddo
     ndxsave = ndxsave + 1
  enddo
  !      write( *, * ) ( a(1, j ), j = 1, bufcols )
  !      write( *, * )( buffer( i ), i = 1, bufrows * bufcols )
  return
end subroutine ftn_transpose_real4
