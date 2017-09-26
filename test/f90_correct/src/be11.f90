!* Copyright (c) 1998, NVIDIA CORPORATION.  All rights reserved.
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

! Equivalence in modules

        module xxx
        integer i(2)
        integer n1, n2
        equivalence (i(1),n1), (i(2),n2)
        end module
        use xxx
	integer result(4),expect(4)
	data expect/3,99,3,99/
        n1 = 3
	i(2) = 99
	result(1)=n1
	result(2)=n2
	result(3)=i(1)
	result(4)=i(2)
	call check(result,expect,4)
        end
