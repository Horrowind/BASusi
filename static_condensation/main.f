program StaticCondensation

use utilities
implicit none


! variables

type(Point), dimension(:,:), allocatable :: V    ! grid
real(RP) :: a, b ! grid size
real(RP), dimension(:,:), allocatable :: g    ! RHS

integer  :: n, m ! number of elements in x, y direction; total number: 2*m*n (regular grid)
integer  :: uni
integer :: i, j

read(*,*)n
read(*,*)m
read(*,*)a
read(*,*)b

allocate(V(0:n, 0:m), g(0:n, 0:m))

call regular_grid(V, n, m, a, b)
g = sin(2 *pi*V%x) * sin(2*pi*V%y)
!write(*,*) V
!write(*,*) g

open(newunit=uni, file = "output")
do i = 0, m
   do j = 0, n
      write(uni, '(100(E12.5,1X))', advance='no') g(j,i)
   end do
   write(uni, *)
end do
close(uni)
deallocate (V, g)

end program StaticCondensation
