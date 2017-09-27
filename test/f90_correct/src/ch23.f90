! Copyright (c) 2004, NVIDIA CORPORATION.  All rights reserved.
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
! assumed-length character pointer array passed to an assumed-length,
! assumed-shape character array - incorrect length, exposed in tonto.
!

  program main
     character(LEN=13), dimension(3) :: the_keys
     character(LEN=13), dimension(:), pointer :: keys
     integer, save :: result(3)
     integer       :: expect(3) = (/1,2,3/)

     interface
       subroutine create_copy(self,vec)
       character(*), dimension(:) :: self
       pointer :: self
       character(*), dimension(:) :: vec
       end subroutine
     end interface

     the_keys(1) = "Brent"
     the_keys(2) = "Dudley"
     the_keys(3) = "Leback"
!     print *,the_keys(1)
!     print *,the_keys(2)
!     print *,the_keys(3)
     call create_copy(keys,the_keys)
!     print *,keys(1)
!     print *,keys(2)
!     print *,keys(3)
      do i = 1, 3
	 if (the_keys(i) == keys(i)) then
	    result(i) = i
	 endif
      enddo
      call check(result, expect, 3)
   end

  subroutine create_copy(self,vec)
    character(*), dimension(:) :: self
    ! Make a copy of "vec"
    pointer :: self
    character(*), dimension(:) :: vec
    integer i

    interface
      subroutine create(self,dim)
      character(*), dimension(:) :: self
      pointer :: self
      integer(kind=kind(1)) :: dim
      end subroutine

      subroutine copy(self,vec)
      character(*), dimension(:) :: self
      character(*), dimension(:) :: vec
      end subroutine
    end interface

    i = size(vec)
    call create(self,i)
    call copy(self,vec) !!!!length was 0, needs to be from the descriptor
   end subroutine

  subroutine create(self,dim)
    character(*), dimension(:) :: self
    ! Create space for a string vector
    pointer :: self
    integer(kind=kind(1)) :: dim
    nullify(self)
    allocate(self(dim))
   end subroutine

  subroutine copy(self,vec)
    character(*), dimension(:) :: self
    ! Make a copy of "vec"
      character(*), dimension(:) :: vec
      integer(kind=kind(1)) :: i
      do i = 1,size(vec)
         call copys(self(i),vec(i))
      end do
   end subroutine

   subroutine copys(self,s)
    character(*) :: self
    ! Make a copy of the string "s"
      character(*) :: s
      self = s
   end subroutine
