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


subroutine ftn_mvmul_real4( ta, m, k, alpha, a, lda, b, beta, c )
  implicit none
#ifdef TARGET_64
  integer*8 :: m, k, lda
#else
  integer   :: m, k, lda
#endif
  real*4      :: alpha, beta
  real*4, dimension( lda, * ) :: a
  real*4, dimension( * )      :: b, c
  integer   :: ta
  ! Local variables
  
#ifdef TARGET_64
  integer*8   :: i, j, kk
#else
  integer     :: i, j, kk
#endif
  real*4      :: temp

  if( ta .eq. 0 )then ! normally oriented a matrix
     if( beta .ne. 0 )then
        do i = 1, m
           c( i ) = beta * c( i ) 
        enddo
     else
        do i = 1, m
           c( i ) = 0.0 
        enddo
     endif
     do kk = 1, k
        temp = alpha * b( kk )
        do i = 1, m
           c( i ) = c( i ) + a( i, kk ) * temp
        enddo
     enddo
  else ! matrix a is transposed - may be improved with buffering of b * alpha
     do i = 1, m
        c( i ) = c( i ) * beta
        temp = 0.0
        do kk = 1, k
           temp = temp + a( kk, i ) * b( kk )
        enddo
        c( i ) = c( i ) + alpha * temp
     enddo
  endif
  return
end subroutine ftn_mvmul_real4

