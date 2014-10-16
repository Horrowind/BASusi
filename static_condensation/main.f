program StaticCondensation

implicit none
use module utilities

! constants

integer, parameter ::  RP = selected_real_kind(14) ! real precisionRP

! variables

type Points, dimension(:,:), allocatable :: V    ! grid

real(RP) :: a, b ! grid size


integer  :: n, m ! number of elements in x, y direction; total number: 2*m*n (regular grid)


open(newunit = uni, file = input)
  read(uni,*)n
  read(uni,*)m
  read(uni,*)a
  read(uni,*)b
close(uni)  
allocate(V(0:n, 0:m))

call regular_grid(V, n, m, a, b)

deallocate (V)

end program StaticCondensation
