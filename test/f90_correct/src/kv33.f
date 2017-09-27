c Copyright (c) 2016, NVIDIA CORPORATION.  All rights reserved.
c
c Licensed under the Apache License, Version 2.0 (the "License");
c you may not use this file except in compliance with the License.
c You may obtain a copy of the License at
c
c     http://www.apache.org/licenses/LICENSE-2.0
c
c Unless required by applicable law or agreed to in writing, software
c distributed under the License is distributed on an "AS IS" BASIS,
c WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
c See the License for the specific language governing permissions and
c limitations under the License.
c

	double precision a0(100), a1(100)
	double precision b0(100), b1(100)

	do i = 1,100
	 a0(i) = i
	 a1(i) = 3
	 b0(i) = 0
	 b1(i) = 0
	 b1(i) = mod(a0(i),a1(i))
	 b0(i) = mod(a0(i),a1(i))
	enddo

	call checkd( b0, b1, 100 )
	end
