** Copyright (c) 1989, NVIDIA CORPORATION.  All rights reserved.
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

*   Compiler directives: %list, %nolist, %eject. include statement

C   NONSTANDARD:
C     VMS directives %LIST, %NOLIST, %ELECT, and VMS INCLUDE stmt.

%list
%LIST  
	common /c/ rslts(6), expect(6)
	integer rslts, expect
	data expect / 1, 2, 3, 4, 5, 6/

%nolist
C

	rslts(1)
&          = 1
%list
	Include 'ac00.i'
	INCLUDE 'ac00.h'
%eject
	rslts(5) = 5
	rslts(6) = if(5)
	rslts(3) = 3
	call check(rslts, expect, 6)
	end
c  comment lines between two subprograms ...

	include 'ac00.h3'
%nolist
