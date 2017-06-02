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

subroutine ftn_mvmul_cmplx8( ta, tb, m, k, alpha, a, lda, b, beta, c )
  implicit none
#ifdef TARGET_64
  integer*8 :: m, k, lda
#else
  integer   :: m, k, lda
#endif
  complex*8      :: alpha, beta
  complex*8, dimension( lda, * ) :: a
  complex*8, dimension( * )      :: b, c
  integer   :: ta, tb
  ! Local variables
  
#ifdef TARGET_64
  integer*8   :: i, j, kk
#else
  integer     :: i, j, kk
#endif
  complex*8      :: temp
 
!  print *, "#### In mvmul ####"
  if( beta .ne. 0 )then
     do i = 1, m
        c( i ) = beta * c( i ) 
     enddo
  else
     do i = 1, m
        c( i ) = 0.0 
     enddo
  endif

  if( ta .eq. 0 )then ! normally oriented a matrix
     if( tb .ne. 2 )then
        do i = 1, m
           do kk = 1, k
              c( i ) = c( i ) + alpha * a( i, kk ) * b( kk )
           enddo
        enddo
     else
        do i = 1, m
           do kk = 1, k
              c( i ) = c( i ) + alpha * a( i, kk ) * conjg( b( kk ) )
           enddo
        enddo
     endif
  else ! matrix a is transposed - may be improved with buffering of b * alpha
     if( ta .ne. 2 )then ! a not conjugated
        if( tb .ne. 2 )then ! b not conjugated
           do i = 1, m
              temp = 0.0
              do kk = 1, k
                 temp = temp + a( kk, i ) * b( kk )
              enddo
              c( i ) = c( i ) + alpha * temp
           enddo
        else ! b is conjugated
           do i = 1, m
              temp = 0.0
              do kk = 1, k
                 temp = temp + a( kk, i ) * conjg( b( kk ) )
              enddo
              c( i ) = c( i ) + alpha * temp
           enddo
        endif
     else ! a is conjugated
        if( tb .ne. 2 )then ! b not conjugated
           do i = 1, m
              temp = 0.0
              do kk = 1, k
                 temp = temp + conjg( a( kk, i ) ) * b( kk )
              enddo
              c( i ) = c( i ) + alpha * temp
           enddo
        else ! b is conjugated
           do i = 1, m
              temp = 0.0
              do kk = 1, k
                 temp = temp + conjg(  a( kk, i ) ) * conjg( b( kk ) )
              enddo
              c( i ) = c( i ) + alpha * temp
           enddo
        endif
     endif
  endif
  return
end subroutine ftn_mvmul_cmplx8

