program variationequation
% das ist am Ende gar nicht linear.
implicit none

  integer,   parameter      :: RNP = kind(1.0d0)
  real(RNP), parameter      :: ONE = 1.0, TWO = 2.0, FOUR = 4.0
  real(RNP), parameter      :: PI = 3.14159265359

  integer                   :: i, uni,j                 ! loop/output variables

  real(RNP), allocatable    :: x_h(:), u_h(:), f_h(:) ! grid, unknown, RHS
  real(RNP), allocatable    :: m(:,:), k(:,:)         ! mass matrix, stiffness matrix
  real(RNP), allocatable    :: B(:,:)                 ! system matrix

  integer        :: NE=5                  ! Number of elements > 2

  open (newunit = uni, file = 'equationsystem')



     allocate (x_h(0:NE), u_h(1:NE-1), f_h(1:NE-1))
     Allocate (m(1:NE-1,1:NE-1),k(1:NE-1,1:NE-1), B(1:NE-1,1:3))
     
     B = 0
     
     !RHS
     do i = 1, NE-1
        x_h(i)= real(i)/real(NE)                ! set the grid
        f_h(i)= (4*PI**2+1)*sin(x_h(i) *2*PI)   ! we will get u(x) =
        ! sin(2*PI*x) as solution
     end do
     
  call trapezregel(f_h, NE)                 !integration
  
  !LHS
  m = mass_matrix(NE)
  k = stiffness_matrix(NE)

  write(*, '(4(E12.5,1X))') k+m
  
! solves the equation system by transforming the tridiagonal matrix to vector
  B = MatrixToVector(k+m)
  call thomas_algorithm(B,f_h, u_h)
  
  ! output with boundary conditions
  
  write (uni,'(2(E12.5,1X))') 0.0, 0.0          !BC
  do i = 1, NE-1
     write (uni,'(2(E12.5,1X))') u_h(i), x_h(i)
  end do
  write (uni,'(2(E12.5,1X))') 0.0, 1.0          !BC
  deallocate(x_h, u_h, f_h, m, k, B)

close (uni)
contains ! ----------------------------------------
  
function mass_matrix(NE) result (m)

  real(RNP)                 :: m(1:NE-1, 1:NE-1)
  integer, intent (in)      :: NE
  integer                   :: i

  m = 0

  do i = 2, NE - 2
      m(i, i-1:i+1) = [ONE, FOUR, ONE]
  end do

  m(1, 1:2) = [FOUR, ONE]
  m(NE-1, NE-2:NE-1) = [ONE, FOUR]

  m = real(m) / real(NE * 6)

end function mass_matrix

function stiffness_matrix(NE) result (k)

  real(RNP)                 :: k(1:NE-1, 1:NE-1)
  integer, intent (in)      :: NE
  integer                   :: i

  k = 0

  do i = 2, NE - 2
    k(i, i-1:i+1) = [-ONE, TWO, -ONE]
  end do

  k(1, 1:2) = [TWO, -ONE]
  k(NE-1, NE-2:NE-1) = [-ONE, TWO]

  k = k * real(NE)

end function stiffness_matrix

subroutine trapezregel (f_h, NE)

  real(RNP), intent(inout)  :: f_h(0:)
  integer, intent(in)       :: NE
  f_h = f_h/real(NE,RNP)

end subroutine trapezregel

function MatrixToVector (A) result (x)

  real(RNP), intent(in)    :: A (1:,1:)
  real(RNP)                :: x (1:size(A,1),1:3)
  integer                  :: i, N

  N = size(A,1)

  x(1,:) = [real(0.0,RNP), A(1,1:2)]
  x(N,:) = [A(N, N-1:N),real(0.0,RNP)]
  do i = 2, N-1
    x(i,:) = A(i, i-1:i+1)
  end do

end function MatrixToVector


subroutine thomas_algorithm(M, d, x)

  real(RNP)     :: M(:,:), x(:), d(:)
  integer       :: i, NE

  NE = size(M,1)

  M(1,3)= M(1,3)/M(1,2)
  d(1)= d(1)/M(1,2)

!foreward iteration
  do i = 2, NE
    M(i,3) = M(i,3)/(M(i,2)-M(i-1,3)*M(i,1))
    d(i) = (d(i) - d(i-1)*M(i,1))/(M(i,2)-M(i-1,3)*M(i,1))
  end do
!backward iteration
  x(NE) = d(NE)

  do i = NE-1, 1, -1
    x(i) = d(i)-M(i,3)*x(i+1)
  end do

end subroutine thomas_algorithm

end program variationequation
