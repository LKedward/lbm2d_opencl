module LBM_Tecplot_Output
!! Contains routines for writing macroscopic variables to tecplot files
!
!
use iso_fortran_env, only: sp=> real32
implicit none

contains

subroutine writePltTxt(filename,ni,nj,nSave,dtSave,dx,rho,u,v)
  !! Write macroscopic variable data to tecplot ascii format
  !
  !
  character(*), intent(in) :: filename
  integer, intent(in) :: ni, nj, nSave
  real(sp), intent(in) :: dtSave ! Time interval between saved solutions
  real(sp), intent(in) :: dx     ! Lattice spacing
  real(sp), intent(in), dimension(ni*nj,nSave) :: rho, u, v

  integer, parameter :: nperline = 100

  integer :: fh, n, i, j

  open(newunit=fh,file=filename,status='unknown')

  write(fh,*) 'TITLE = "LBM Solution"'
  write(fh,*) 'VARIABLES = "X" "Y" "rho" "u" "v"'

  do n=1,nSave
    
    write(fh,'(A,I8,A,I8,A,F10.4,A)',advance='no') 'ZONE T="Solution" DATAPACKING=BLOCK I=',nj,' J=',ni, &
               ' STRANDID=1 SOLUTIONTIME=',n*dtSave, 'VARLOCATION=([1-5]=NODAL)'

    if (n==1) then
      ! Write grid data 
      write(fh,*) ''
      do i=1,ni
        do j=1,nj
          write(fh,*) (i-1)*dx
        end do
      end do
    
      do i=1,ni
        do j=1,nj
          write(fh,*) (j-1)*dx
        end do
      end do

    else
      ! Use grid data from zone 1
      write(fh,*) 'VARSHARELIST=([1,2]=1)'

    end if

    write(fh,*) ( rho(i:min(i+nperline-1,ni*nj),n),NEW_LINE('A') , i=1,ni*nj,nperline )
    write(fh,*) ( u(i:min(i+nperline-1,ni*nj),n),NEW_LINE('A') , i=1,ni*nj,nperline )
    write(fh,*) ( v(i:min(i+nperline-1,ni*nj),n),NEW_LINE('A') , i=1,ni*nj,nperline )

  end do

  close(fh)

end subroutine writePltTxt
! -----------------------------------------------------------------------------


subroutine writePltBin(filename,ni,nj,nSave,dtSave,dx,rho,u,v)
  !! Write macroscopic variable data to tecplot binary format
  !
  !
  use iso_c_binding, only: int32=>c_int32_t, float32=>c_float, float64=>c_double
  
  character(*), intent(in) :: filename
  integer, intent(in) :: ni, nj, nSave
  real(sp), intent(in) :: dtSave ! Time interval between saved solutions
  real(sp), intent(in) :: dx     ! Lattice spacing
  real(sp), intent(in), dimension(ni*nj,nSave) :: rho, u, v

  integer, parameter :: nVar = 5 
  character(*), parameter :: pltTitle = 'LBM Solution'
  character(*), parameter :: zoneName = 'Solution'
  character(*), parameter :: v1Name = 'rho'
  character(*), parameter :: v2Name = 'u'
  character(*), parameter :: v3Name = 'v'
  
  integer :: fh, n, i, j

  ! Open file for binary access
  open(newunit=fh,file=filename,status='unknown',access='stream')

  ! File header
  write(fh) '#!TDV112'
  write(fh) int(1,int32) ! byte order
  write(fh) int(0,int32) ! file type: full
  write(fh) (ichar(pltTitle(i:i),int32),i=1,len(pltTitle)),0 ! Title

  write(fh) int(nVar,int32) ! no. of vars

  ! Variable names
  write(fh) ichar('x',int32),0,ichar('y',int32),0
  write(fh) (ichar(v1Name(i:i),int32),i=1,len(v1Name)),0
  write(fh) (ichar(v2Name(i:i),int32),i=1,len(v2Name)),0
  write(fh) (ichar(v3Name(i:i),int32),i=1,len(v3Name)),0

  ! Zone header info
  do n=1,nSave
    write(fh) real(299.0,float32)         ! Zone start
    write(fh) (ichar(zoneName(i:i),int32),i=1,len(zoneName)),0
    write(fh) int(-1,int32)               ! no parent zones
    write(fh) int(0,int32)               !static strand id
    write(fh) real(n*dtSave,float64)          ! Zone time
    write(fh) int(-1,int32)               ! Not used by tecplot
    write(fh) int(0,int32)                ! ordered zone
    write(fh) int(0,int32)                ! data packing block
    write(fh) int(0,int32)                ! don't specify var loc
    write(fh) int(0,int32)                ! no neighbours
    write(fh) nj,ni,1                     ! Imax, Jmax, Kmax
    write(fh) int(0,int32)                ! no aux data 
  end do
  write(fh) real(357.0,float32)           ! end of header

  ! Data section
  do n=1, nSave
    write(fh) real(299.0,float32)         ! zone start
    write(fh) (int(1,int32),i=1,nVar)     ! Variable type (float)
    write(fh) int(0,int32)                ! no passive vars
    if (n==1) then
      write(fh) int(0,int32)                   ! no variable sharing
      write(fh) -1                             ! no connectivity sharing
      write(fh) real(0.0,float64)              ! Zone variable bounds
      write(fh) real(ni*dx,float64)
      write(fh) real(0.0,float64)
      write(fh) real(nj*dx,float64)
      write(fh) real(minval(rho(:,n)),float64)
      write(fh) real(maxval(rho(:,n)),float64)
      write(fh) real(minval(u(:,n)),float64)
      write(fh) real(maxval(u(:,n)),float64)
      write(fh) real(minval(v(:,n)),float64)
      write(fh) real(maxval(v(:,n)),float64)

      do i=1,ni                                ! Write grid data out (block format)
        do j=1,nj
          write(fh) real((i-1)*dx,float32)
        end do
      end do
    
      do i=1,ni
        do j=1,nj
          write(fh) real((j-1)*dx,float32)
        end do
      end do

      write(fh) real(rho(:,n),float32)         ! Write variable data out (block format)
      write(fh) real(u(:,n),float32)
      write(fh) real(v(:,n),float32)
    else
      write(fh) int(1,int32)                   ! variable sharing enabled
      write(fh) int([0,0,-1,-1,-1],int32)         ! Use x,y from first zone
      write(fh) -1                             ! no connectivity sharing
      write(fh) real(minval(rho(:,n)),float64)
      write(fh) real(maxval(rho(:,n)),float64)
      write(fh) real(minval(u(:,n)),float64)
      write(fh) real(maxval(u(:,n)),float64)
      write(fh) real(minval(v(:,n)),float64)
      write(fh) real(maxval(v(:,n)),float64)

      write(fh) real(rho(:,n),float32)         ! Write variable data out (block format)
      write(fh) real(u(:,n),float32) 
      write(fh) real(v(:,n),float32)
    end if
  end do

  close(fh)

end subroutine writePltBin
! -----------------------------------------------------------------------------



end module LBM_Tecplot_output