!* Copyright (c) 2018, NVIDIA CORPORATION.  All rights reserved.
!*
!* Licensed under the Apache License, Version 2.0 (the "License");
!* you may not use this file except in compliance with the License.
!* You may obtain a copy of the License at
!*
!*     http://www.apache.org/licenses/LICENSE-2.0
!*
!* Unless required by applicable law or agreed to in writing, software
!* distributed under the License is distributed on an "AS IS" BASIS,
!* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
!* See the License for the specific language governing permissions and
!* limitations under the License.

program gppkernel

!      implicit none
      integer :: ngpown
!      integer :: my_igp


!$OMP PARALLEL  
       ngpown = 2
!$OMP END PARALLEL

!$OMP PARALLEL  default(firstprivate)
      ngpown = 1
!$OMP END PARALLEL

    print *, "PASS"

end program
