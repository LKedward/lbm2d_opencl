program lbmocl

use LBM_Test_Cases
use LBM_User_Input, only: getUserInput, printConfiguration
use LBM_Tecplot_Output
use Focal
use iso_fortran_env, only: sp=>real32, dp=>real64
implicit none

! ------------------ VARIABLE DECLARATIONS ------------------

! VARS: Constants
integer, parameter :: q = 9            !! Number of lattice velocity vectors
real(sp), parameter :: AR = 4.0         !! Domain aspect ratio for cylinder and step cases
real(sp), parameter :: D = 80           !! Cylinder diameter, grid units
real(sp), parameter :: HSTEP = 0.25    !! Size of step % of NJ

! VARS: User input
integer :: caseNum
integer :: nIter, saveFreq, outputMode, it, nSave
real(sp) ::  dx_o, dt_o, tau_o, omega, cVel
character(100) :: outputFile, cl_vendor

! VARS: Case configuration
integer :: ni, nj                        !! Grid dimensions
integer :: npts                          !! Always ni*nj
real(sp) :: dx                           !! Grid spacing
real(sp) :: dt                           !! Time step
real(sp) :: tau                          !! Viscous relaxation parameter
integer, allocatable :: bcFlags(:)       !! Boundary condition flags (ni x nj)
real(sp), allocatable :: velocities(:)   !! Velocity boundary condition vectors (2 x n)

! VARS: OpenCL runtime (Focal)
integer :: blockSize(2), globalSize(2)
character(:), allocatable :: programSource
type(fclDevice), allocatable :: devices(:)
type(fclCommandQ) :: asyncQ
type(fclProgram) :: prog
type(fclKernel) :: initK, collideK, boundariesK, streamK, calcVarsK
type(fclEvent) :: depends(2)
type(fclProfiler) :: profiler

! VARS: Device buffers
type(fclDeviceFloat) :: distFun_d, distFun2_d, velocities_d
type(fclDeviceFloat) :: rho_d, u_d, v_d
type(fclDeviceInt32) :: bcFlags_d

! VARS: Host arrays
real(sp), pointer :: rho(:), u(:), v(:)
real(sp), pointer :: rho2(:,:), u2(:,:), v2(:,:)

! VARS: Profiling
integer(c_int64_t) :: collideTavg, boundariesTavg, streamTavg
real(dp) :: Tupdate

! ------------------ END DECLARATIONS ------------------

! Process inputs
call getUserInput(caseNum,ni,nIter,saveFreq,outputFile,outputMode,&
                         dx_o, dt_o, tau_o, cl_vendor)

select case(caseNum)
case (1)
  call get_case_lid_cavity(ni,nj,npts,dx,dt,tau,bcFlags,velocities)
case (2)
  call get_case_cylinder(ni,D,AR,nj,npts,dx,dt,tau,bcFlags,velocities)
case (3)
  call get_case_step(ni,AR,HSTEP,nj,npts,dx,dt,tau,bcFlags,velocities)
end select

! Process case config overrides
if (dx_o > 0) then
  dx = dx_o
end if
if (dt_o > 0) then
  dt = dt_o
end if
if (tau_o > 0) then
  tau = tau_o
end if

if (saveFreq > 0) then
  nSave = nIter/saveFreq ! Number of timesteps to save
else
  nSave = 0
end if
cVel = dx/dt           ! Lattice velocity
omega = dt/tau;        ! Relaxation coefficient

call printConfiguration(caseNum,ni,nj,nIter,saveFreq,outputFile,outputMode,&
  dx, dt, tau)


! Create context with specified vendor
call fclSetDefaultContext(fclCreateContext(vendor=trim(adjustl(cl_vendor))))

! Select device with most cores and create command queue
devices = fclFindDevices(sortBy='cores')
call fclSetDefaultCommandQ(fclCreateCommandQ(devices(1),enableProfiling=.true.))

write(*,*) '  Created OpenCL command queue on device: "',devices(1)%name,'"'
write(*,'(A,I6,A,I6,A,I4,A,A,A)') '    (', devices(1)%nComputeUnits,' cores, ', &
    devices(1)%global_memory/1024/1024,'MB, ', &
    devices(1)%clock_freq, 'MHz, ',&
    devices(1)%version,')'
write(*,*) ''

! Setup profiler
profiler%device = devices(1)

! Create an additional asynchronous command queue for host transfers
asyncQ = fclCreateCommandQ(devices(1),enableProfiling=.true.,outOfOrderExec=.true.,blockingRead=.false.)

! Allocate host arrays using pinned (non-paged) memory
call fclAllocHost(rho,npts*nSave)
call fclAllocHost(u,npts*nSave)
call fclAllocHost(v,npts*nSave)

rho2(1:npts,1:nSave) => rho(:)
u2(1:npts,1:nSave) => u(:)
v2(1:npts,1:nSave) => v(:)

! Load and compile OpenCL source
call fclGetKernelResource(programSource)
prog = fclCompileProgram(programSource) !,'-cl-nv-verbose')
! call fclDumpBuildLog(prog,devices(1))

! Calculate global work size as multiple of desired block size
blockSize = [64,1]
globalSize(1) = (ni/blockSize(1) + min(1,mod(ni,blockSize(1))))*blockSize(1)
globalSize(2) = (nj/blockSize(2) + min(1,mod(nj,blockSize(2))))*blockSize(2)

initK = fclGetProgramKernel(prog,'initialise',globalSize,blockSize)
collideK = fclGetProgramKernel(prog,'collide',globalSize,blockSize)
boundariesK = fclGetProgramKernel(prog,'boundaryConditions',globalSize,blockSize)
streamK = fclGetProgramKernel(prog,'stream',globalSize,blockSize)
calcVarsK = fclGetProgramKernel(prog,'macroVars',globalSize,blockSize)

call profiler%add(nIter,initK,collideK,boundariesK,streamK,calcVarsK)

! Initialise buffers on device
call fclInitBuffer(distFun_d,q*npts)
call fclInitBuffer(distFun2_d,q*npts)
call fclInitBUffer(velocities_d,size(velocities,1),access='r')
call fclInitBuffer(asyncQ,rho_d,npts,access='w',profileName='rho_d')
call fclInitBuffer(asyncQ,u_d,npts,access='w',profileName='u_d')
call fclInitBuffer(asyncQ,v_d,npts,access='w',profileName='v_d')
call fclInitBuffer(bcFlags_d,npts,access='r',profileName='bcFlags_d')

call profiler%add(nSave,rho_d,u_d,v_d,bcFlags_d)

! Transfer boundary condition data to device
bcFlags_d = bcFlags
velocities_d = velocities

! Call kernel to initialise distribution function
call initK%launch(ni,nj,distFun_d)
depends(1) = fclLastKernelEvent

! --------- Start unsteady interation ---------
do it = 1,nIter

  call collideK%launch(ni,nj,cVel,omega,distFun_d,bcFlags_d)

  call boundariesK%launch(ni,nj,distFun_d,bcFlags_d,velocities_d)

  call streamK%launch(ni,nj,distFun_d,distFun2_d,bcFlags_d)

  ! Swap new and old distribution pointers
  call fclBufferSwap(distFun_d,distFun2_d)

  if (saveFreq > 0) then
    if (mod(it,saveFreq) == 0) then

      write(*,*) 'Iteration ',it

      ! Calculate macroscopic variables
      ! (Occurs in-order on main queue, with dependency on last data transfer)
      call fclSetDependency(depends(1))
      call calcVarsK%launch(ni,nj,cVel,distFun_d,rho_d,u_d,v_d,bcFlags_d)
      depends(2) = fclLastKernelEvent

      ! Transfer to host using out-of-order asynchronous queue
      ! (Two dependencies: calcVars kernel and last data transfer)
      call fclSetDependency(asyncQ,depends,hold=.true.)
      rho2(:,it/saveFreq) = rho_d
      u2(:,it/saveFreq) = u_d
      v2(:,it/saveFreq) = v_d
      call fclClearDependencies(asyncQ)
      call fclClearDependencies()

      ! Barrier: next transfer depends on completion of last
      call fclBarrier(asyncQ)
      depends(1) = asyncQ%lastBarrierEvent

    end if
  end if

end do
! --------- End iteration ---------

! Wait for all device operations to complete
call fclWait()
call fclWait(asyncQ)

! Calculate and print out profiling data
call fclDumpProfileData(profiler)
call fclDumpTracingData(profiler,'lbm.trace')

! Calculate performance in MLUPS
! (Million lattice updates per second)
collideTavg = sum(fclGetEventDurations(collideK%profileEvents(1:niter)))/niter
boundariesTavg = sum(fclGetEventDurations(boundariesK%profileEvents(1:niter)))/niter
streamTavg = sum(fclGetEventDurations(streamK%profileEvents(1:niter)))/niter

Tupdate = collideTavg+boundariesTavg+streamTavg ! nanoseconds
write(*,'(A,F10.2,A)') '  LBM Average performance: ',npts/(Tupdate*1d-3),' MLUPS'

! Save to file
if (saveFreq > 0) then
  write(*,*) 'saving...'
  if (outputMode == 1) then
    call writePltBin(outputFile,ni,nj,nSave,1.0*saveFreq,dx,rho2,u2,v2)
  else !if (outputMode == 2) then
    call writePltTxt(outputFile,ni,nj,nSave,1.0*saveFreq,dx,rho2,u2,v2)
  end  if
end if

! Deallocate pinned arrays
call fclFreeHost(rho)
call fclFreeHost(u)
call fclFreeHost(v)

end program lbmocl
