! Copyright (c) 2018, NVIDIA CORPORATION.  All rights reserved.
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

! do concurrent locality list

integer, parameter :: N = 6
integer, target    :: a(N,N,N) ! operand via p
integer            :: r(N,N)   ! result, unspecified locality
integer            :: s(N,N)   ! shared locality
integer            :: t(N,N)   ! local locality
integer, pointer   :: p(:,:,:) ! local_init locality

p => a

do concurrent (integer(kind=1)::i=N:1:-1)
  do j = 1,N
    a(i,j,:) = 2*(i+j)
    s(i,j)   = -i-j
  enddo
enddo

do concurrent (integer(2)::i=1:N,j=1:N,i.ne.j) local(t) local_init(p) shared(s)
  do k=1,N
    do concurrent (m=1:N)
      t(k,m) = p(k,m,k)
    enddo
  enddo
  r(i,j) = t(i,j) + s(i,j)
enddo

! print*, r !    0    3    4    5    6    7
            !    3    0    5    6    7    8
            !    4    5    0    7    8    9
            !    5    6    7    0    9   10
            !    6    7    8    9    0   11
            !    7    8    9   10   11    0  -->  sums to 210

if (sum(r).ne.210) print*, 'FAIL'
if (sum(r).eq.210) print*, 'PASS'

end
