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

module class_Circle
    implicit none
    private
    real :: pi = 3.1415926535897931d0 ! Class-wide private constant

    type, public :: Circle
       real :: radius
     contains
       procedure, public, pass :: area =>  circle_area
       procedure, public, pass :: print =>  circle_print
    end type Circle
 contains
    function circle_area(this) result(area)
      class(Circle), intent(in) :: this
      real :: area
      area = pi * this%radius**2
    end function circle_area

    subroutine circle_print(this)
      class(Circle), intent(in) :: this
      real :: area
      area = this%area()  ! Call the type-bound function
      print *, 'Circle: r = ', this%radius, ' area = ', area
    end subroutine circle_print
 end module class_Circle


 program circle_test
USE CHECK_MOD
    use class_Circle
    implicit none
    logical results(1), expect(1)
    real r

    type(Circle) :: c     ! Declare a variable of type Circle.
    c = Circle(1.5)       ! Use the implicit constructor, radius = 1.5.
    call c%print          ! Call the type-bound subroutine

    results = .false.
    expect = .true.

    r = c%area()
    results(1) = Ceiling(r) .eq. Ceiling(7.0685835)  

    call check(results,expect,1)
 end program circle_test


