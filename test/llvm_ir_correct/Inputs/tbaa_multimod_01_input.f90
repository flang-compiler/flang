! A part of the test provided in tbaa_multimod_01.f90

subroutine modify1(arr)
  implicit none
  real, intent(inout) :: arr(:)
  arr(0) = arr(0) + 0.5
end subroutine modify1

subroutine modify2(arr)
  implicit none
  real, intent(inout) :: arr(:)
  arr(2) = arr(2) + 1.5
end subroutine modify2

subroutine printout(arr)
  implicit none
  real, intent(in) :: arr(:)
  integer :: i
  do i = 1, size(arr)
    print arr(i), " "
  enddo
end subroutine printout
