! A part of the test provided in tbaa_multimod_02.f90

subroutine to_load(arr)
  implicit none
  real, intent(inout) :: arr(:)
  real :: var
  var = arr(0)
  var = var * 2
  print var
end subroutine to_load
