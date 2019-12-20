module ConfigLoader_h
  !! Header module for ConfigLoader

  !! @note This is a header module: it contains subroutine interface definitions only.
  !! Subroutine implementation (code) is found in the corresponding submodule file (with no '_h'). @endnote

  use, intrinsic :: iso_fortran_env, sp=>real32, dp=>real64
  implicit none

  integer, parameter :: MAXSTR = 500
    !! Maximum string length
  integer, parameter :: array_fixed = 1
    !! arrayMode: Fixed size, all values must be specified in input
  integer, parameter :: array_pad = 2
    !! arrayMode: Padded array, pad unspecified values with last specified value
  integer, parameter :: array_fill = 3
    !! arrayMode: Fill in array, pad unspecified values from default array

  character(len=1) :: configCommentChar = '#'
    !! Character indicating comment line

  character(len=1) :: configKVDelim = '='
    !! Character delimiting key from value

  interface addConfigParameter
    module subroutine addScalarParameter(keyString,targetVar,defaultVal,enumStrings)
      !! Generic interface for registering scalar parameters
      character(*), intent(in) :: keyString
        !! Parameter key
      class(*), target, intent(inout) :: targetVar
        !! Variable to store parameter once read
      class(*), optional, intent(in) :: defaultVal
        !! Default value of optional parameter, omit for mandatory parameters
      character(*), optional, intent(in) :: enumStrings(:)
        !! Array of strings for enumeration parameters (for integer parameters only)
    end subroutine addScalarParameter

    module subroutine addArrayParameter(keyString,targetVar,arrayMode,defaultVal)
      !! Generic interface for registering array parameters
      character(*), intent(in) :: keyString
        !! Parameter key
      class(*), target, intent(inout) :: targetVar(:)
        !! Variable to store parameter once read
      integer, intent(in) :: arrayMode
        !! Determine how to pad array: [[ConfigLoader_h:array_fixed]], [[ConfigLoader_h:array_pad]], [[ConfigLoader_h:array_fill]]
      class(*), optional, intent(in) :: defaultVal(:)
        !! Default value of optional parameter, omit for mandatory parameters
    end subroutine addArrayParameter
  end interface

  interface
    module subroutine parseConfigFile(filename)
      !! Open and parse a configuration file for registered parameters
      character(*), intent(in) :: filename
        !! File path of config file to parse
    end subroutine parseConfigFile
  end interface

end module ConfigLoader_h
