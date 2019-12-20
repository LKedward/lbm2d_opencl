module LBM_User_Input
use ConfigLoader_h,  only: addConfigParameter, parseConfigFile
use iso_fortran_env, only: sp=>real32
implicit none

contains

subroutine getUserInput(caseNum,ni,nIter,saveFreq,outputFile,outputMode,&
                         dx, dt, tau, cl_vendor)
  !! Subroutine to read an input file for user config
  !!
  integer, intent(out) :: caseNum            ! Preset case number
  integer, intent(out) :: ni                 ! No. of points in i direction
  integer, intent(out) :: nIter              ! No. of iterations to perform
  integer, intent(out) :: saveFreq           ! Frequency of plotting solution
  character(100), intent(out) :: outputFile  ! Tecplot filename
  integer, intent(out) :: outputMode         ! Output mode, 1:binary, 2:ascii
  real(sp), intent(out) :: dx                ! Override lattice spacing
  real(sp), intent(out) :: dt                ! Override timestep
  real(sp), intent(out) :: tau               ! Override viscosity parameter
  character(100), intent(out) :: cl_vendor   ! OpenCL device vendor string

  ! --- Locals ---
  integer :: nArg, temp
  character(:), allocatable :: configFile

  ! Check for first command argument
  nArg = command_argument_count()
  if (narg < 1) then
    write(*,*) '(!) lbm_ocl: Expecting config filename as first argument.'
    stop
  end if

  ! Get first command argument
  call get_command_argument(1,length=temp)
  allocate(character(temp) :: configFile)
  call get_command_argument(1,configFile)

  ! Setup config file parameters to read in
  call addConfigParameter('CASE',caseNum,defaultVal=1, &
                            enumStrings=['CAVITY  ','CYLINDER','STEP    '])
  call addConfigParameter('NI',ni,defaultVal=400)
  call addConfigParameter('N_ITER',nIter,defaultVal=5000)
  call addConfigParameter('SAVE_FREQ',saveFreq,defaultVal=100)
  call addConfigParameter('SAVE_FILE',outputFile,defaultVal='lbm.plt')
  call addConfigParameter('SAVE_TYPE',outputMode,defaultVal=1, &
                            enumStrings=['BIN','TXT'])

  call addConfigParameter('DX',dx,defaultVal=-1.0)
  call addConfigParameter('DT',dt,defaultVal=-1.0)
  call addConfigParameter('TAU',tau,defaultVal=-1.0)
  
  call addConfigParameter('CL_VENDOR',cl_vendor,defaultVal='nvidia')
  
  ! Parse config file
  !  (Throws error if configFile not found)
  call parseConfigFile(configFile)

end subroutine getUserInput
! -----------------------------------------------------------------------------


subroutine printConfiguration(caseNum,ni,nj,nIter,saveFreq,outputFile,outputMode,&
  dx, dt, tau)
  !! Subroutine to print the current program configuration
  !!
  integer, intent(in) :: caseNum            ! Preset case number
  integer, intent(in) :: ni                 ! No. of points in i direction
  integer, intent(in) :: nj                 ! No. of points in j direction
  integer, intent(in) :: nIter              ! No. of iterations to perform
  integer, intent(in) :: saveFreq           ! Frequency of plotting solution
  character(100), intent(in) :: outputFile  ! Tecplot filename
  integer, intent(in) :: outputMode         ! Output mode, 1:binary, 2:ascii
  real(sp), intent(in) :: dx                ! Lattice spacing
  real(sp), intent(in) :: dt                ! Timestep
  real(sp), intent(in) :: tau               ! Viscosity parameter

  integer :: i

  write(*,*) ('-',i=1,77)
  write(*,*) '                   D2Q9 BGK Lattice Boltzmann Code (OpenCL)'
  write(*,*) ''
  write(*,*) '                              L. J. Kedward 2019'
  write(*,*) ('-',i=1,77)
  write(*,*) ''

  select case(caseNum)
  case (1)
    write(*,'(A,I5,I5)') '  Running test case: lid-driven CAVITY , NI,NJ = ',ni,nj
  case (2)
    write(*,'(A,I5,I5)') '  Running test case: flow around a CYLINDER, NI,NJ = ',ni,nj
  case (3)
    write(*,'(A,I5,I5)') '  Running test case: flow over a STEP, NI,NJ = ',ni,nj
  end select

  write(*,'(A,F6.4,A,F6.4,A,F6.4)') '  Lattice spacing: ', dx, '    Timestep: ',dt,'    Tau: ',tau

  if (saveFreq > 0) then
    write(*,'(A,I8,A,I4)') '  Maximum iterations: ', nIter, '    Save frequency: ',saveFreq
    if (outputMode == 1) then
      write(*,'(A,A,A)') '  Output file: ',trim(outputFile),'    Output mode: tecplot binary'
    else !if(outputMode == 2)
      write(*,'(A,A,A)') '  Output file: ',trim(outputFile),'    Output mode: tecplot ASCII'
    end if
  else
    write(*,'(A,I8,A)') '  Maximum iterations: ', nIter, '    Saving disabled'
  end if
  
  write(*,*) ''

end subroutine printConfiguration
! -----------------------------------------------------------------------------


subroutine readCmdFlagKV(argKey, argValue, found)
  !!  Read Key-Value parameter from short format command-line flags
  !!
  !!  Example: command: 'program.exe -n 8'
  !!           code usage: call flagReadKV('n',N)
  !!
  character(*), intent(in) :: argKey
    !! key string used to identify parameter
  character(*), intent(out), optional :: argValue
    !! parameter value as string
  logical, intent(out), optional :: found
    !! indicates whether argument was found

  integer :: count, i, argLen
  character(500) :: arg

  if (present(found)) found = .False.

  count = command_argument_count()
  if (present(argValue)) count = count - 1

  do i=1,count
    call get_command_argument(i,arg,argLen)
    if (index(arg,'-') == 1) then
      if (index(upperstr(arg),upperstr(argKey)) > 0) then
        if (present(found)) found = .True.
        if (present(argValue)) call get_command_argument(i+1,argValue)
        exit
      end if
    end if
  end do

  return

end subroutine readCmdFlagKV
! -----------------------------------------------------------------------------


function upperStr(linei)
  !! Return copy of string converted to uppercase
  !! Used for case-insensitive string comparison
  character(len=*),intent(in) :: linei
    !! Input string to convert to uppercase
  character(len=len(linei)) upperstr
    !! Converted string output

  intrinsic ichar, char, len
  integer :: inlen ! number of characters in trimmed input string
  integer :: i10 ! counter to increment through input and output string
  integer :: ilet ! current character being converted represented using ASCII Decimal Equivalent

  inlen=len_trim(linei) ! number of characters to convert to uppercase
  upperstr=' '  ! initialize output string to all blanks

  if(inlen.gt.len(upperstr))then ! make sure there is room to store the output characters
      write(*,'(a)')'*ufpp* FATAL - OUTPUT TOO LONG TO CONVERT TO UPPERCASE:'
  endif

  ! loop through each character in input string
  do i10=1,inlen,1
      ilet=ichar(linei(i10:i10))                ! current character in input to convert to output converted to ADE
      if( (ilet.ge.97) .and. (ilet.le.122))then ! lowercase a-z in ASCII is 97 to 122; uppercase A-Z in ASCII is 65 to 90
      upperstr(i10:i10)=char(ilet-32)        ! convert lowercase a-z to uppercase A-Z
      else
      upperstr(i10:i10)=linei(i10:i10)       ! character is not a lowercase a-z, just put it in output
      endif
  enddo

end function upperStr
! -----------------------------------------------------------------------------

end module LBM_User_Input
