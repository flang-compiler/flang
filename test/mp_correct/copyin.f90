! RUN: %flang %s -fopenmp -fsyntax-only

module somemod
  integer, allocatable :: something(:)
  !$OMP THREADPRIVATE(something)
end module somemod

program some
  use somemod

  !$OMP PARALLEL COPYIN(something)
  !$OMP END PARALLEL
end program some
