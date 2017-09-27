** Copyright (c) 1989-2017, NVIDIA CORPORATION.  All rights reserved.
**
** Licensed under the Apache License, Version 2.0 (the "License");
** you may not use this file except in compliance with the License.
** You may obtain a copy of the License at
**
**     http://www.apache.org/licenses/LICENSE-2.0
**
** Unless required by applicable law or agreed to in writing, software
** distributed under the License is distributed on an "AS IS" BASIS,
** WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
** See the License for the specific language governing permissions and
** limitations under the License.

*   Register allocation bug when passing arguments by value to a
*   STDCALL *   routine on win32.  Even though STDCALL is only available on
*   win32, this test can still be used on other systems; there will not be
*   any value passing, but that's ok.

        integer function ifoo( hInstance, nCmdShow)
!DEC$ ATTRIBUTES STDCALL :: ifoo
        integer hInstance
        integer nCmdShow
        ii = hinstance
        jj = ncmdshow
        call bar(ii,jj)
        ifoo = 0
        end
        subroutine bar(ii,jj)
	common/ires/ires(2)
	ires(1) = ii
	ires(2) = jj
        end
	common/ires/ires(2)
	integer iexp(2)
	data iexp/1,2/
        integer ifoo
        external ifoo
!DEC$ ATTRIBUTES STDCALL :: ifoo
        kk = ifoo(1,2)
	call check(ires, iexp, 2)
        end

