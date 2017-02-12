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


subroutine ftn_vmmul_real4( tb, n, k, alpha, a, b, ldb, beta, c )
  implicit none
#ifdef TARGET_64
  integer*8 :: n, k, ldb
#else
  integer   :: n, k, ldb
#endif
  integer   :: tb
  real*4, dimension (ldb, * ) :: b
  real*4, dimension ( * )     :: a, c
  real*4                      :: alpha, beta

! local variables
#ifdef TARGET_64
  integer*8 :: i, j, kk
#else
  integer   :: i, j, kk
#endif
  real*4    :: temp        

  if( beta .ne. 0.0 )then
     do i = 1, n
        c( i ) = beta * c( i )
     enddo
  else
     do i = 1, n
        c( i ) = 0.0
     enddo
  end if
  if( tb .eq. 0 ) then !b is normally oriented
     if( alpha .eq. 1.0 )then
        do j = 1, n
           do kk = 1, k
              c( j ) = c( j ) + a( kk ) * b( kk, j )
           enddo
        enddo
     elseif( alpha .eq. -1.0 )then
        do j = 1, n
           do kk = 1, k
              c( j ) = c( j ) - a( kk ) * b( kk, j )
           enddo
        enddo
     else
        do j = 1, n
           do kk = 1, k
              c( j ) = c( j ) + alpha * a( kk ) * b( kk, j )
           enddo
        enddo
     endif
  else
     do kk = 1, k
        temp = alpha * a( kk )
        do j = 1, n
           c( j ) = c( j ) + temp * b( j, kk )
        enddo
     enddo
  endif
  return
end subroutine ftn_vmmul_real4
