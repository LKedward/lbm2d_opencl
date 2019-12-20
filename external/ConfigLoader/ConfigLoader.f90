submodule (ConfigLoader_h) ConfigLoader
  !! Provides routines for parsing a configuration file

  implicit none


  integer, parameter :: NMAX = 100
    !! Maximum number of config parameters (for preallocation)
  integer, parameter :: MAXENUM = 10
    !! Maximum number of enumerators for enumeration parameters

  type param
    !! Type structure for config parameters

    integer :: dim                            !! Array parameter dimension
    character(MAXSTR) :: keyString            !! Parameter config key
    character(MAXSTR) :: enumStrings(MAXENUM) !! Array of strings for enumeration parameters
    integer :: nEnum                          !! No. of string-enumerations

    class(*), pointer :: targetVar            !! Pointer to scalar target variable
    class(*), pointer :: targetArr(:)         !! Pointer to array target variable
    logical :: isMandatory                    !! Is parameter mandatory?
    integer :: arrayMode                      !! How to treat config input shorter than target var.

    logical :: readIn                         !! Has parameter been read in?

  end type param

  integer :: nParam
    !! Number of parameters registered

  type(param) :: parameters(NMAX)
    !! Array of parameter registered parameters

  contains


  module procedure addScalarParameter
    !! Specific routine for registering scalar parameters

    integer :: i

    nParam = nParam + 1
    i = nParam

    if (present(enumStrings)) then
      select type(t=>targetVar)
      type is (integer)

      class default
        call error_enum_wrongtype()
      end select
      if (size(enumStrings,1) > MAXENUM) then
        call error_enum_toobig()
      end if

      parameters(i)%enumStrings(1:size(enumStrings,1)) = enumStrings
      parameters(i)%nEnum = size(enumStrings,1)
      parameters(i)%dim = -1 ! string-enumerated integer

    else
      parameters(i)%dim = 0 ! Scalar
    end if

    parameters(i)%readIn = .false.
    parameters(i)%keyString = keyString
    parameters(i)%targetVar => targetVar
    parameters(i)%isMandatory = .not.present(defaultVal)

    ! Set default values if provided
    if (present(defaultVal)) then

      select type(t=>targetVar)

      type is (integer)
        select type(d=>defaultVal)
        type is (integer)
          t = d
        class default
          call error_type_mismatch()
        end select

      type is (real(sp))
        select type(d=>defaultVal)
        type is (real(sp))
          t = d
        class default
          call error_type_mismatch()
        end select

      type is (real(dp))
        select type(d=>defaultVal)
        type is (real(dp))
          t = d
        class default
          call error_type_mismatch()
        end select

      type is (logical)
        select type(d=>defaultVal)
        type is (logical)
          t = d
        class default
          call error_type_mismatch()
        end select

      type is (character(*))
        select type(d=>defaultVal)
        type is (character(*))
          t = d
        class default
          call error_type_mismatch()
        end select

      class default
        call error_unsupported_type()

      end select

    end if

  end procedure addScalarParameter
  ! ---------------------------------------------------------------------------


  module procedure addArrayParameter
    !! Specific routine for registering array parameters
    integer :: i,n,m

    if (.not.present(defaultVal) .and. arrayMode == array_fill) then
      call error_arrayfill_default()
    end if

    nParam = nParam + 1
    i = nParam

    n = size(targetVar,1)

    parameters(i)%dim = n
    parameters(i)%readIn = .false.
    parameters(i)%keyString = keyString
    parameters(i)%targetArr => targetVar
    parameters(i)%arrayMode = arrayMode
    parameters(i)%isMandatory = .not.present(defaultVal)

    ! Set default values if provided
    if (present(defaultVal)) then

      m = size(defaultVal,1)

      if (m /= n) then
        call error_arraydefault_size()
      end if

      select type(t=>targetVar)

      type is (integer)
        select type(d=>defaultVal)
        type is (integer)
          t = d
        class default
          call error_type_mismatch()
        end select

      type is (real(sp))
        select type(d=>defaultVal)
        type is (real(sp))
          t = d
        class default
          call error_type_mismatch()
        end select

      type is (real(dp))
        select type(d=>defaultVal)
        type is (real(dp))
          t = d
        class default
          call error_type_mismatch()
        end select

      type is (logical)
        select type(d=>defaultVal)
        type is (logical)
          t = d
        class default
          call error_type_mismatch()
        end select

      type is (character(*))
        select type(d=>defaultVal)
        type is (character(*))
          t = d
        class default
          call error_type_mismatch()

          end select
      class default
        call error_unsupported_type()

      end select
    end if

  end procedure addArrayParameter
  ! ---------------------------------------------------------------------------


  module procedure parseConfigFile
    !! Open and parse a configuration file for registered parameters
    integer :: i, j, i1, linenum, nArr
    integer :: fh, fstat
    logical :: fExist, parsed, missingParam, needPadding
    character(MAXSTR) :: lineString, tempString

    ! --- Check file ---
    inquire(file=trim(filename), exist=fExist)
    if (.not.fExist) then
      call error_file_notfound(filename)
    end if

    linenum = 1
    open(newunit=fh,file=filename)
    read(fh,'(A)',IOSTAT=fstat) lineString
    do while (fstat == 0)

      if (index(adjustl(lineString),configCommentChar) == 1) then
        ! Comment line: do nothing

      else if (len(trim(adjustl(lineString))) == 0) then
        ! Blank line: do nothing

      else

        parsed = .false.
        do i=1,nParam ! Loop over registered parameters

          if (configLHS(lineString,trim(parameters(i)%keyString))) then
            ! Parameter key matched

            ! Check if already parsed
            if (parameters(i)%readIn) then
              call error_parse_repeatedparam(linestring,linenum,i)
            end if

            i1 = index(lineString, configKVDelim)

            parameters(i)%readIn = .true.
            parsed = .true.

            if (parameters(i)%dim == -1) then
              ! Expecting string-enumerated scalar integer

              ! Check number of items
              nArr = ntokens(trim(lineString(i1+1:)))
              if (nArr > 1) then
                call error_parse_arraydim(linestring,linenum,i)
              end if

              select type(t=>parameters(i)%targetVar)
              type is (integer)

                t = -1

                read(lineString(i1+1:),*,ERR=300) tempString
                if (len(trim(tempString)) == 0) then
                  call error_parse(linestring,linenum,i)
                else
                  do j=1,parameters(i)%nEnum ! Loop through enum strings for match

                      if ( upperstr(trim(adjustl(tempString))) == &
                        upperstr(trim(adjustl(parameters(i)%enumStrings(j)))) ) then
                        t = j
                      end if
                  end do
                end if

                if (t < 0) then
                  call error_parse_enum(linestring,linenum,i)
                end if

              end select

            else if (parameters(i)%dim == 0) then
              ! Expecting scalar parameter

              ! Check number of items
              nArr = ntokens(trim(lineString(i1+1:)))
              if (nArr > 1) then
                call error_parse_arraydim(linestring,linenum,i)
              end if

              select type(t=>parameters(i)%targetVar)
              type is (integer)
                read(lineString(i1+1:),*,ERR=300,END=300) t

              type is (real(sp))
                read(lineString(i1+1:),*,ERR=300,END=300) t

              type is (real(dp))
                read(lineString(i1+1:),*,ERR=300,END=300) t

              type is (logical)
                t = parseLogical(lineString,linenum,i)

              type is (character(*))
                read(lineString(i1+1:),'(A)',ERR=300,END=300) t

              end select

            else
              ! Expecting array parameter

              ! Check number of items
              nArr = ntokens(trim(lineString(i1+1:)))

              if ( (parameters(i)%arrayMode == array_fixed .and. (nArr /= parameters(i)%dim)) .or. &
                     nArr > parameters(i)%dim ) then
                call error_parse_arraydim(linestring,linenum,i)
              end if

              needPadding = (parameters(i)%arrayMode == array_pad .and. (nArr < parameters(i)%dim))

              select type(t=>parameters(i)%targetArr)
              type is (integer)
                read(lineString(i1+1:),*,ERR=300,END=201) t
201             if (needPadding) then
                  t(nArr+1:parameters(i)%dim) = t(nArr)
                end if

              type is (real(sp))
                read(lineString(i1+1:),*,ERR=300,END=205) t
205             if (needPadding) then
                  t(nArr+1:parameters(i)%dim) = t(nArr)
                end if

              type is (real(dp))
                read(lineString(i1+1:),*,ERR=300,END=202) t
202             if (needPadding) then
                  t(nArr+1:parameters(i)%dim) = t(nArr)
                end if

              type is (logical)
                read(lineString(i1+1:),*,ERR=300,END=203) t
203             if (needPadding) then
                  t(nArr+1:parameters(i)%dim) = t(nArr)
                end if

              type is (character(*))
                read(lineString(i1+1:),'(A)',ERR=300,END=204) t
204             if (needPadding) then
                  t(nArr+1:parameters(i)%dim) = t(nArr)
                end if

              end select

            end if ! enum, scalar, or array

          end if ! param matched

        end do ! registered parameters loop

        ! Check line was parsed
        if (.not.parsed) then
          call error_parse(linestring,linenum)
        end if

      end if ! comment line, blank line or data line

      linenum = linenum + 1
      read(fh,'(A)',IOSTAT=fstat) lineString

    end do ! file readline loop

    close(fh)

    ! Check mandatory parameters were read in
    missingParam = .false.
    do i=1,nParam
      if (parameters(i)%isMandatory .and. .not. parameters(i)%readIn) then
        missingParam = .true.
        write(*,*) '(!) ConfigLoader - mandatory parameter "',trim(parameters(i)%keyString),'" not found.'
      end if
    end do

    if (missingParam) then
        stop
    end if

    return

300 call error_parse(linestring,linenum,i)

  end procedure parseConfigFile
  ! ---------------------------------------------------------------------------


  logical function configLHS(inputString,keyString)
    !! Parse left-hand-side of key=val config string
    !! Returns true if inputString contains form "keyString = somevalue"
    character(len=*) :: inputString
    character(len=*) :: keyString

    integer :: i1, i2, i3, i4, i5

    i1 = index(upperstr(inputString), upperstr(keyString))
    i2 = i1+len(keyString)
    i3 = index(inputString(i2:i2),' ')
    i4 = index(inputString(i2:i2),configKVDelim)
    i5 = index(inputString,configKVDelim)

    configLHS = (i1>0).AND.((i3>0).OR.(i4>0)).AND.(i5>0)

  end function configLHS
  ! ---------------------------------------------------------------------------


  logical function parseLogical(inputString,linenum,paramIdx)
    !! Parse key=val where val is logical
    !!  Returns true if val is "YES"
    !!  Throws error if val is neither "YES" nor "NO"
    character(*), intent(in) :: inputString
    integer, intent(in) :: linenum
    integer, intent(in) :: paramIdx

    integer :: i1, iy, in
    character(maxstr) :: val

    i1 = index(inputString, configKVDelim)
    read(inputString(i1+1:),*,END=301) val

    iy = index(upperstr(val), 'YES')
    in = index(upperstr(val), 'NO')

    if (in == 0.AND.iy == 0) then
      call error_parse(inputString,linenum,paramIdx)
    end if

    parseLogical = (iy > 0)

    return

301 call error_parse(inputString,linenum,paramIdx)

  end function parseLogical
  ! ---------------------------------------------------------------------------


  function upperstr(linei)
    !! Return copy of string converted to uppercase
    !! Used for case-insensitive string comparison
    !! 1996, John S. Urban http://fortranwiki.org/fortran/show/ufp
    character(len=*),intent(in) :: linei               ! input string to convert to uppercase
    character(len=len(linei)) upperstr

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
  end function upperstr
  ! --------------------------------------------------------------------


  integer function ntokens(line)
    !! Number of data columns in string
    !! @note Source unknown @endnote
      character,intent(in):: line*(*)
      integer i, n, toks

      i = 1;
      n = len_trim(line)
      toks = 0
      ntokens = 0
      do while(i <= n)
         do while(line(i:i) == ' ')
           i = i + 1
           if (n < i) return
         enddo
         toks = toks + 1
         ntokens = toks
         do
           i = i + 1
           if (n < i) return
           if (line(i:i) == ' ') exit
         enddo
      enddo

  end function ntokens
  ! --------------------------------------------------------------------


  subroutine error_file_notfound(filename)
    character(*), intent(in) :: filename
    write(*,*) '(!) ConfigLoader - file "',trim(filename),'" not found.'
    stop
  end subroutine error_file_notfound
  ! ---------------------------------------------------------------------------


  subroutine error_type_mismatch()
    write(*,*) '(!) ConfigLoader - defaultVal type does not match targetVar type.'
    stop
  end subroutine error_type_mismatch
  ! ---------------------------------------------------------------------------


  subroutine error_unsupported_type()
    write(*,*) '(!) ConfigLoader - targetVar is of unsupported type.'
    stop
  end subroutine error_unsupported_type
  ! ---------------------------------------------------------------------------


  subroutine error_enum_wrongtype()
    write(*,*) '(!) ConfigLoader - targetVar not an integer. String enumeration can only be used with integer parameter variables.'
    stop
  end subroutine error_enum_wrongtype
  ! ---------------------------------------------------------------------------


  subroutine error_enum_toobig()
    write(*,*) '(!) ConfigLoader - too many enumeration strings specified. Increase MAXENUM.'
    stop
  end subroutine error_enum_toobig
  ! ---------------------------------------------------------------------------


  subroutine error_arrayfill_default()
    write(*,*) '(!) ConfigLoader - arrayMode:array_fill requires a default value to be specified.'
    write(*,*) 'For mandatory arguments, omit default value and use array_pad or array_fixed .'
    stop
  end subroutine error_arrayfill_default
  ! ---------------------------------------------------------------------------


  subroutine error_arraydefault_size()
    write(*,*) '(!) ConfigLoader - size of array defaultVal does not match target array variable.'
    stop
  end subroutine error_arraydefault_size
  ! ---------------------------------------------------------------------------


  subroutine error_parse(linestring,linenum,paramIdx)
    character(*), intent(in) :: linestring
    integer, intent(in) :: linenum
    integer, optional, intent(in) :: paramIdx   !! parameter index, omit for general input error

    if (present(paramIdx)) then

      write(*,*) '(!) ConfigLoader - error parsing parameter "',trim(parameters(paramIdx)%keyString),'"'

      select type(t=>parameters(paramIdx)%targetVar)

      type is (integer)
        write(*,'(A)',advance='no') '     Expecting integer, '
      type is (real(dp))
        write(*,'(A)',advance='no') '     Expecting double precision real, '
      type is (logical)
        write(*,'(A)',advance='no') "     Expecting logical 'YES' or 'NO', "
      type is (character(*))
        write(*,'(A)',advance='no') '     Expecting string, '

      end select

    else

      write(*,*) '(!) ConfigLoader - unparsed configuration input.'

    end if

    write(*,'(A,A,A,I4,A)') 'got: "',trim(linestring),'" (line ',linenum,')'

    stop
  end subroutine error_parse
  ! ---------------------------------------------------------------------------


  subroutine error_parse_enum(linestring,linenum,paramIdx)
    character(*), intent(in) :: linestring
    integer, intent(in) :: linenum
    integer, intent(in) :: paramIdx

    integer :: i

    write(*,*) '(!) ConfigLoader - error parsing enumerated parameter "',trim(parameters(paramIdx)%keyString),'"'
    write(*,'(A)',advance='no') '    Expecting one of: '
    do i=1,parameters(paramIdx)%nEnum
      write(*,'(A,A,A)',advance='no') "'",trim(parameters(paramIdx)%enumStrings(i)),"' "
    end do
    write(*,*)
    write(*,'(A,A,A,I4,A)') '    got: "',trim(linestring),'" (line ',linenum,')'
    stop

  end subroutine error_parse_enum
  ! ---------------------------------------------------------------------------


  subroutine error_parse_repeatedparam(linestring,linenum,paramIdx)
    character(*), intent(in) :: linestring
    integer, intent(in) :: linenum
    integer, intent(in) :: paramIdx

    write(*,*) '(!) ConfigLoader - parsed error, parameter "',trim(parameters(paramIdx)%keyString), &
                 '" specified more than once.'
    write(*,'(A,I4,A,A,A)') ' found repeat on line',linenum,' : "',trim(linestring),'"'
    stop

  end subroutine error_parse_repeatedparam
  ! ---------------------------------------------------------------------------


  subroutine error_parse_arraydim(linestring,linenum,paramIdx)
    character(*), intent(in) :: linestring
    integer, intent(in) :: linenum
    integer, intent(in) :: paramIdx

    if (parameters(paramIdx)%dim < 1) then
      write(*,'(A,A,A)') '(!) ConfigLoader - parse error, expecting only 1 value for scalar parameter "',&
                 trim(parameters(paramIdx)%keyString),'"'
    else if (parameters(paramIdx)%arrayMode == array_fixed) then
      write(*,'(A,I2,A,A,A)') '(!) ConfigLoader - parse error, expecting ',parameters(paramIdx)%dim, &
                ' values for array parameter "',trim(parameters(paramIdx)%keyString),'"'
    else
      write(*,'(A,I2,A,A,A)') '(!) ConfigLoader - parse error, expecting up to ',parameters(paramIdx)%dim, &
                ' values for array parameter "',trim(parameters(paramIdx)%keyString),'"'
    end if
    write(*,'(A,I4,A,A,A)') '    error on line',linenum,' : "',trim(linestring),'"'
    stop

  end subroutine error_parse_arraydim
  ! ---------------------------------------------------------------------------


end submodule ConfigLoader
