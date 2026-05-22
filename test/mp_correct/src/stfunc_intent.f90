program main
    integer :: a
    a = 10
    call test_stfunc(a)
    print *, "PASS"
contains
subroutine test_stfunc(a)
    implicit none
    integer, intent(in) :: a
    integer :: b, c, i
    integer :: func1
    integer :: arr(10)
    func1(b, c) = a
!$omp parallel do
    do i = 1, 10
        arr(i) = func1(b, c)
    end do
end subroutine
end
