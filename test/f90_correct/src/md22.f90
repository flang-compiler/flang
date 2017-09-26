!
! Copyright (c) 1999, NVIDIA CORPORATION.  All rights reserved.
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
!       Testing Gen_block with module - must run with  4 processors
!       data initialization or array parameter in module


        module foo
	integer gba(4)
	data gba /1,1,1,1/
	integer, parameter, dimension(4)::gbb=(/1,1,1,1/)
        integer a(4),b(4)
!hpf$ distribute a(gen_block(gba))
!hpf$ distribute b(gen_block(gbb))
	contains
	subroutine s(n)
	integer ca(4),cb(4)
!hpf$ distribute ca(gen_block(gba))
!hpf$ distribute cb(gen_block(gbb))
	ca = a*n
	cb = ca
	b = cb
	end subroutine s
	subroutine t(n)
	call tt(n)
	contains
	subroutine tt(n)
	integer da(4),db(4)
!hpf$ distribute da(gen_block(gba))
!hpf$ distribute db(gen_block(gbb))
	da = a*n
	db = da
	b = db
	end subroutine tt
	end subroutine t

        end module

        module foo2
        use foo
        integer ea(4),eb(4)
!hpf$ distribute ea(gen_block(gba))
!hpf$ distribute eb(gen_block(gbb))
        end module

        program p
        use foo2
	integer result(32),expect(32)
        data expect/1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,3,             &
     &		6,9,12,1,2,3,4,2,4,6,8/
	forall(i=1:4) a(i) = i
	forall(i=1:4) b(i) = i
	forall(i=1:4) ea(i) = i
	forall(i=1:4) eb(i) = i
        !print *, a,b
        !print *, ea,eb
	result(1:4) = a
	result(5:8) = b
	result(9:12) = ea
	result(13:16) = eb
	call s(3)
	!print *, a,b
	result(17:20) = a
	result(21:24) = b
	call t(2)
	!print *, a,b
	result(25:28) = a
	result(29:32) = b
	call check(result,expect,32)
        end
