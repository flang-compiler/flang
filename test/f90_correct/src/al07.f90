! Copyright (c) 2010, NVIDIA CORPORATION.  All rights reserved.
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
! tests source= clause

module al07mod
   type singly_linked_list
      type(singly_linked_list), pointer :: next => null()
      integer :: data
   end type singly_linked_list

contains

   recursive function sll_copy(src) result(copy)
      type(singly_linked_list), intent(in) :: src
      type(singly_linked_list), pointer :: copy
      allocate(copy, source=src)
      if (associated(src%next)) copy%next => sll_copy(src%next)
   end function sll_copy

   subroutine sll_print(list)
      type(singly_linked_list), target, intent(in) :: list
      type(singly_linked_list), pointer :: ptr
      print *
      ptr => list
      do
         if (.not. associated(ptr)) exit
         print *, ptr%data, associated(ptr%next)
         ptr => ptr%next
      end do
   end subroutine sll_print

end module al07mod


use al07mod

type(singly_linked_list), target :: source
type(singly_linked_list), pointer :: dest, ptr, next
integer, dimension(10) :: res
integer, dimension(10) :: exp = (/ 1,2,3,4,5,6,7,8,9,10 /)

! Initialize source list to 1..10
source%data = 1
ptr => source
do i = 2, 10
   allocate(next)
   next%data = i
   ptr%next => next
   ptr => next
end do

!call sll_print(source)

! Build a new list using the source= clause
dest => sll_copy(source)

! Zeroing out the original list should have no effect on the new list
ptr => source
do while (associated(ptr))
   ptr%data = 0
   ptr => ptr%next
end do

!call sll_print(dest)

ptr => dest
i = 1
do while (associated(ptr))
   res(i) = ptr%data
   ptr => ptr%next
   i = i + 1
end do

call check(res, exp, 10)

end
