program main
    print *, "PASS"
end

subroutine test_stfunc
    implicit none
    type test
        integer :: c
    end type
    type(test) :: a
    integer :: b, i
    integer :: func1
    integer :: arr(10)
    func1(b) = a%c
!$omp parallel do
    do i = 1, 10
        arr(i) = func1(b)
    end do
end subroutine
