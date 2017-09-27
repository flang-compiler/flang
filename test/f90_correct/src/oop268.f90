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

module shape_mod

type shape
        integer :: color
        logical :: filled
        integer :: x
        integer :: y
contains
	procedure :: write => write_shape 
	procedure :: draw => draw_shape
end type shape

type, EXTENDS ( shape ) :: rectangle
        integer :: the_length
        integer :: the_width
contains
        procedure,pass(this) :: write => write_rec
	procedure :: draw => draw_rectangle
	procedure :: coord
        procedure :: coordinate
end type rectangle

type, extends (rectangle) :: square
	type(rectangle),allocatable :: my_rect
contains
        procedure :: draw => draw_sq
	procedure :: write => write_sq
	procedure,pass(this) :: write2 => write_sq2
	procedure :: write3 => write_sq3

end type square
contains

  subroutine write_shape(this,results,i)
   class (shape) :: this
   logical results(:)
   integer i
   type(shape) :: sh
   results(i) = .true.!same_type_as(sh,this)
   end subroutine write_shape

   subroutine write_rec(this,results,i)
   class (rectangle) :: this
   logical results(:)
   integer i
   type(shape) :: sh
   results(i) = .true.!same_type_as(sh,this)
   end subroutine write_rec

   subroutine draw_shape(this,results,i)
   class (shape) :: this
   logical results(:)
   integer i
   print *, 'draw shape!'
   end subroutine draw_shape

   subroutine draw_rectangle(this,results,i)
   class (rectangle) :: this
   logical results(:)
   integer i
   type(rectangle) :: rec
   results(i) = .true.!extends_type_of(this,rec)
   end subroutine draw_rectangle

   subroutine write_sq(this,results,i)
   class (square) :: this
   logical results(:)
   integer i
   type(rectangle) :: rec
   results(i) = .true.!extends_type_of(this,rec)
   end subroutine write_sq

   subroutine draw_sq(this,results,i)
   class (square) :: this
   logical results(:)
   integer i
   type(rectangle) :: rec
   results(i) = .true.!extends_type_of(this,rec)
   end subroutine draw_sq

   subroutine write_sq2(i,this,results)
   class (square) :: this
   integer i 
   logical results(:)
   type(rectangle) :: rec
   results(i) = .true.!extends_type_of(this,rec)
   end subroutine write_sq2

   logical function write_sq3(this)
   class (square) :: this
   type(rectangle) :: rec
   write_sq3 = .true.!extends_type_of(this,rec)
   end function 

    function coordinate(this) result(return_y)
    class(rectangle)       ,intent(in)  :: this
    real ,dimension(:) ,allocatable :: return_y
    allocate(return_y(10))
    return_y = 0
  end function

   function coord(this,y)
   class(rectangle)       ,intent(in)  :: this
   real ,dimension(:) :: y
   type(rectangle) :: coord

   coord = this
   coord%color = 9
   coord%filled = .true.
   coord%x = 10
   coord%y = 20
   coord%the_length = 11
   coord%the_width = 21
   

   end function

   
   


end module shape_mod

program p
USE CHECK_MOD
use shape_mod

logical l 
logical results(11)
logical expect(11)
class(square),allocatable :: s
class(shape),allocatable :: sh
type(rectangle) :: r
type(square),allocatable :: sq

expect = .true.
results = .false.

allocate(s)
call s%write2(1,results)
call s%write(results,2)
call s%draw(results,3)

allocate(sq)
results(5) = sq%write3()

allocate(sh)
call write_shape(sh,results,4)

allocate(s%my_rect)
r = s%rectangle%coord(s%my_rect%coordinate())

results(6) = r%color .eq. 9
results(7) = r%filled
results(8) = r%x .eq. 10
results(9) = r%y .eq. 20
results(10) = r%the_length .eq. 11
results(11) = r%the_width .eq. 21

call check(results,expect,11)

end


