*
* Copyright (c) 2015, NVIDIA CORPORATION.  All rights reserved.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*     http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*
* This tests # characters in fixed-form fortran code
* # in the initial column should be ignored, and treated as a comment even when
* preprocessing

#if 0
#error "This should never get called"
#endif

	program test
	print *, "Hello"
     #, "World"
	call check(.true., .true., 1)
	END
