module LBM_Test_Cases
!! Contains routines for constructing predefined test cases
!
!
use iso_fortran_env, only: sp=> real32
implicit none

! type LBMCase
!   !! Structure type for encapsulating run case definitions
!   integer :: ni, nj                        !! Grid dimensions
!   integer :: npts                          !! Always ni*nj
!   real(sp) :: dx                           !! Grid spacing
!   real(sp) :: dt                           !! Time step
!   real(sp) :: tau                          !! Viscous relaxation parameter
!   integer, allocatable :: bcFlags(:)       !! Boundary condition flags (ni x nj)
!   real(sp), allocatable :: velocities(:)   !! Velocity boundary condition vectors (2 x n)
! end type LBMCase

contains


subroutine get_case_lid_cavity(ni,nj,npts,dx,dt,tau,bcFlags,velocities)
  !! Flow in square lid-driven cavity
  !
  integer, intent(in) :: ni              !! No. of points in i direction
  integer, intent(out) :: nj             !!                  j directions
  integer, intent(out) :: npts           !! Always ni*nj
  real(sp), intent(out) :: dx            !! Grid spacing
  real(sp), intent(out) :: dt            !! Timestep
  real(sp), intent(out) :: tau           !! Viscosity parameter
  integer, intent(out), allocatable :: bcFlags(:)
    !! Boundary condition flags (ni x nj)
  real(sp), intent(out), allocatable :: velocities(:)
    !! Velocity boundary condition vectors (2 x n)

  integer :: i, j

  nj = ni
  npts = ni*nj

  allocate(bcFlags(npts))
  allocate(velocities(2))

  bcFlags = 0
  do i=1,ni
    do j=1,nj

      if (i==1 .or. i==ni .or. j==1) then
        bcFlags( (i-1)*nj+j ) = -1; ! No slip
      end if

      if (j==nj) then
        bcFlags( (i-1)*nj+j ) =  1; ! Moving
      end if

    end do
  end do

  velocities = [0.5, 0.0]
  dx = 0.1
  dt = 1
  tau = 0.6

end subroutine get_case_lid_cavity
! -----------------------------------------------------------------------------


subroutine get_case_cylinder(ni,d,AR,nj,npts,dx,dt,tau,bcFlags,velocities)
  !! Separating flow around a cylinder
  !
  integer, intent(in) :: ni              !! No. of points in i direction
  real(sp), intent(in) :: d              !! Diameter of cylinder
  real(sp), intent(in) :: AR             !! Domain aspect ratio
  integer, intent(out) :: nj             !! No. of points in j direction
  integer, intent(out) :: npts           !! Always ni*nj
  real(sp), intent(out) :: dx            !! Grid spacing
  real(sp), intent(out) :: dt            !! Timestep
  real(sp), intent(out) :: tau           !! Viscosity parameter
  integer, intent(out), allocatable :: bcFlags(:)
    !! Boundary condition flags (ni x nj)
  real(sp), intent(out), allocatable :: velocities(:)
    !! Velocity boundary condition vectors (2 x n)

  integer :: i, j
  real(sp) :: r, radius
  real(sp) :: cx, cy

  radius = d/2
  nj = int(ni/AR)
  npts = ni*nj

  allocate(bcFlags(npts))
  allocate(velocities(2))

  cx = 40+radius
  cy = (nj)/2

  bcFlags = 0
  do i=1,ni
    do j=1,nj

      if (i==1) then ! Inlet
        bcFlags( (i-1)*nj+j ) = 1
      end if

      if (i==ni) then ! Outlet
        bcFlags( (i-1)*nj+j ) = -2 ! Extrapolate
      end if

      if (j==1 .or. j==nj) then
        bcFlags( (i-1)*nj+j ) = -1
      end if


      r = sqrt( (i-cx)**2 + (j-cy)**2 )
      if (r < radius) then
        bcFlags( (i-1)*nj+j ) = -1
      end if

    end do
  end do

  velocities = [0.15, 0.0]
  dx = 1.0
  dt = 0.5
  tau = 0.50001

end subroutine get_case_cylinder
! -----------------------------------------------------------------------------


subroutine get_case_step(ni,AR,h,nj,npts,dx,dt,tau,bcFlags,velocities)  !! Flow over a square step
  !
  integer, intent(in) :: ni              !! No. of points in i direction
  real(sp), intent(in) :: AR             !! Domain aspect ratio, default 4.0
  real(sp), intent(in) :: h              !! Step height % of domain, default = 0.25
  integer, intent(out) :: nj             !! No. of points in j direction
  integer, intent(out) :: npts           !! Always ni*nj
  real(sp), intent(out) :: dx            !! Grid spacing
  real(sp), intent(out) :: dt            !! Timestep
  real(sp), intent(out) :: tau           !! Viscosity parameter
  integer, intent(out), allocatable :: bcFlags(:)
    !! Boundary condition flags (ni x nj)
  real(sp), intent(out), allocatable :: velocities(:)
    !! Velocity boundary condition vectors (2 x n)

  integer :: i, j, nh

  nj = int(ni/AR)
  npts = ni*nj
  nh = int(h*min(ni,nj))

  allocate(bcFlags(npts))
  allocate(velocities(2))

  bcFlags = 0
  do i=1,ni
    do j=1,nj

      if (i==1 .or. i==ni .or. j==nj) then
        bcFlags( (i-1)*nj+j ) =  1; ! Moving
      end if

      if (j==1) then
        bcFlags( (i-1)*nj+j ) =  -1; ! Wall
      end if

    end do
  end do

  do i=1,ni
    do j=1,nj
      ! No slip
      if (i<(ni+nh)/2 .and. i>(ni-nh)/2 .and. j<nh) then
        bcFlags( (i-1)*nj+j ) = -1  ! Wall
      end if
    end do
  end do

  velocities = [0.15, 0.0]
  dx = 0.5
  dt = 0.5
  tau = 0.6

end subroutine get_case_step
! -----------------------------------------------------------------------------


end module LBM_Test_Cases
