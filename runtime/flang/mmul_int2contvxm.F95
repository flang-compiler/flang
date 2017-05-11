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

subroutine F90_matmul_int2_contvxm(dest, src1, src2, m_extent, n_extent)

  DESC_INT n_extent
  DESC_INT m_extent
  INTEGER*2, dimension(m_extent) :: src1
  INTEGER*2, dimension(m_extent,n_extent) :: src2
  INTEGER*2, dimension(n_extent) :: dest

  DESC_INT n
  DESC_INT m

  do n=1,n_extent
    dest(n) = 0
    do m=1,m_extent
      dest(n) = dest(n) + src1(m) * src2(m,n)
    end do
 end do

end subroutine
