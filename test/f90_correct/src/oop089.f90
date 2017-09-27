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
        procedure,pass(this) :: write => write_shape
        procedure :: draw => draw_shape
end type shape

type, EXTENDS ( shape ) :: rectangle
        integer :: the_length
        integer :: the_width
contains
        procedure,pass(this) :: write => write_rec
        procedure :: draw => draw_rectangle
	procedure :: draw2
	procedure,pass(this) :: write2
end type rectangle

type, extends (rectangle) :: square
end type square
interface
subroutine draw2(this,results,i)
   import rectangle
   class (rectangle) :: this
   integer results(:)
   integer i
   end subroutine draw2
end interface
interface
 subroutine write_sq(this,results,i)
   import square
   class (square) :: this
   integer results(:)
   integer i
end subroutine
end interface
interface
  subroutine draw_sq(this,results,i)
   import square
   class (square) :: this
   integer results(:)
   integer i
end subroutine
end interface
interface
subroutine write_sq2(results,i,this)
   import square
   class (square) :: this
   integer i
   integer results(:)
end subroutine
end interface
interface
 subroutine write_shape(this,results,i)
   import shape
   class (shape) :: this
   integer results(:)
   integer i
end subroutine
end interface
interface
subroutine draw_shape(this,results,i)
   import shape,rectangle
   class (shape) :: this
   integer results(:)
   integer i
end subroutine
end interface
interface
subroutine write_rec(this,results,i)
   import rectangle
   class (rectangle) :: this
   integer results(:)
   integer i
end subroutine
end interface
interface
 subroutine draw_rectangle(this,results,i)
   import rectangle
   class (rectangle) :: this
   integer results(:)
   integer i
end subroutine
end interface
interface
subroutine write2(results,i,this,flag)
   import rectangle
   class (rectangle) :: this
   integer results(:)
   integer i
   integer flag
   end subroutine
end interface

end module shape_mod


  subroutine write_shape(this,results,i)
   use shape_mod
   class (shape) :: this
   integer results(:)
   integer i
   type(shape) :: sh
   results(i) = same_type_as(sh,this)
   end subroutine write_shape

   subroutine write_rec(this,results,i)
   use shape_mod
   class (rectangle) :: this
   integer results(:)
   integer i
   type(rectangle) :: rec
   results(i) = same_type_as(rec,this)
   end subroutine write_rec

   subroutine write2(results,i,this,flag)
   use shape_mod
   class (rectangle) :: this
   integer results(:)
   integer i
   integer flag
   type(rectangle) :: rec
   type(square) :: sq
   if (flag .eq. 0) then
      results(i) = same_type_as(rec,this)
   else
      results(i) = same_type_as(this,sq)
   endif
   end subroutine 

   subroutine draw_shape(this,results,i)
   use shape_mod
   class (rectangle) :: this
   integer results(:)
   integer i
   end subroutine draw_shape

   subroutine draw_rectangle(this,results,i)
   use shape_mod
   class (rectangle) :: this
   integer results(:)
   integer i
   type(rectangle) :: rec
   results(i) = same_type_as(this,rec)
   end subroutine draw_rectangle

   subroutine draw2(this,results,i)
   use shape_mod
   class (rectangle) :: this
   integer results(:)
   integer i
   type(rectangle) :: rec
   results(i) = same_type_as(this,rec)
   end subroutine draw2

   subroutine write_sq(this,results,i)
   use shape_mod
   class (square) :: this
   integer results(:)
   integer i
   type(rectangle) :: rec
   results(i) = extends_type_of(this,rec)
   end subroutine write_sq

   subroutine draw_sq(this,results,i)
   use shape_mod
   class (square) :: this
   integer results(:)
   integer i
   type(rectangle) :: rec
   results(i) = extends_type_of(this,rec)
   end subroutine draw_sq

   subroutine write_sq2(results,i,this)
   use shape_mod
   class (square) :: this
   integer i
   integer results(:)
   type(rectangle) :: rec
   type(square) :: sq
   results(i) = extends_type_of(this,rec)
   end subroutine write_sq2



program p
USE CHECK_MOD
use shape_mod

logical l 
integer results(6)
integer expect(6)
data expect /.true.,.false.,.false.,.true.,.true.,.true./
data results /.false.,.true.,.true.,.false.,.false.,.false./
class(square),allocatable :: s
class(shape),allocatable :: sh
type(rectangle) :: r

allocate(s)
s%the_length = 1000
s%color = 1
call s%write2(results,1,1)
call s%write(results,2)
call s%draw(results,3)

call s%rectangle%write2(results,4,0);
call draw2(s%rectangle,results,5)

allocate(sh)
call sh%write(results,6)

call check(results,expect,6)

end


