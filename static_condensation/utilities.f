module utilities

implicit none

! constants

integer, parameter ::  RP = selected_real_kind(14) ! real precisionRP
real(RP), parameter :: pi = 3.14159265358979323
type Point
   real(RP) :: x
   real(RP) :: y
end type Point


contains

! returns a regular grid V with n*m grid points in the domain [0,a]x[0,b]
subroutine regular_grid(V, n, m, a, b)
  type (Point), dimension(0:n,0:m) :: V    ! grid
  real(RP)                         :: a, b ! domain [0,a]x[0,b] 
  integer                          :: n    ! stepwidth 1/n in x direction 
  integer                          :: m    ! stepwidth 1/n in y direction 
  
  integer                          :: i, j ! loop variables

  do i = 0, n
     do j = 0, m
        V(i,j)%x = i*a/n
        V(i,j)%y = j*b/m
     end do
  end do

end subroutine regular_grid

end module utilities
