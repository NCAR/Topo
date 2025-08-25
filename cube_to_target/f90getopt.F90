module f90getopt

    implicit none

    ! Portable declaration of stderr, stdin, stdout
#ifdef f2003
use, intrinsic :: iso_fortran_env, only : stdin=>input_unit, &
                                          stdout=>output_unit, &
                                          stderr=>error_unit
#else
#define stdin  5
#define stdout 6
#define stderr 0
#endif

    character(len=1024):: optarg        ! Option's value
    character        :: optopt        ! Option's character
    integer          :: optind=1      ! Index of the next argument to process
    logical          :: opterr=.true. ! Errors are printed by default. Set opterr=.false. to suppress them

    type option_s
        character(len=80) :: name      ! Name of the option
        logical           :: has_arg   ! Option has an argument (.true./.false.)
        character         :: short     ! Option's short character equal to optopt
        logical           :: specified ! Is option specified on command line
        logical           :: required  ! Is option required
    end type option_s

    ! grpind is index of next option within group; always >= 2
    integer, private:: grpind=2

contains

    ! ----------------------------------------
    ! Return str(i:j) if 1 <= i <= j <= len(str),
    ! else return empty string.
    ! This is needed because Fortran standard allows but doesn't *require* short-circuited
    ! logical AND and OR operators. So this sometimes fails:
    !     if ( i < len(str) .and. str(i+1:i+1) == ':' ) then
    ! but this works:
    !     if ( substr(str, i+1, i+1) == ':' ) then

    character function substr( str, i, j )
        ! arguments
        character(len=*), intent(in):: str
        integer, intent(in):: i, j

        if ( 1 <= i .and. i <= j .and. j <= len(str)) then
            substr = str(i:j)
        else
            substr = ''
        endif
    end function substr


    ! ----------------------------------------
character function getopt(optstring, opts) result(c)
  ! Minimal changes per user requirements:
  ! - Return '?' on any positional token (positionals not allowed).
  ! - End-of-args returns char(0).
  ! - Long opts via process_long; short via process_short.
  implicit none
  character(len=*), intent(in) :: optstring
  type(option_s),    intent(inout) :: opts(:)
  character(len=256) :: arg
  integer :: argc

  c      = achar(0)
  optarg = ''
  argc   = command_argument_count()

  if (optind > argc) then
    c = char(0)
    return
  end if

  call get_command_argument(optind, arg)

  if (len_trim(arg) == 0) then
    optind = optind + 1
    c = char(0)
    return
  end if

  if (arg(1:1) == '-') then
    if (len_trim(arg) >= 2 .and. arg(2:2) == '-') then
      c = process_long(opts, arg)   ! may consume an extra value token internally
      optind = optind + 1           ! consume the option token itself
    else
      c = process_short(optstring, arg)
      optind = optind + 1           ! consume the option token itself
    end if
  else
    ! Positional args are NOT allowed
    optarg = trim(arg)
    optind = optind + 1             ! make progress
    c = '?'
  end if
end function getopt

    ! ----------------------------------------
character function process_long(longopts, arg) result(c_lng)
  ! Minimal changes per user requirements:
  ! - Unknown long option => '?' with optarg=arg
  ! - If has_arg=.false. but user gives "--flag=value" => '?' (malformed)
  ! - If has_arg=.true. accept both "--opt=value" and "--opt value"
  implicit none
  type(option_s), intent(in) :: longopts(:)
  character(len=*), intent(in) :: arg

  character(len=:), allocatable :: name
  integer :: eqpos, i, argc
  character(len=256) :: nextval

  c_lng  = '?'
  optarg = ''

  ! Split "--name[=value]"
  eqpos = index(arg, "=")
  if (eqpos > 0) then
    name = trim(arg(3:eqpos-1))
  else
    name = trim(arg(3:))
  end if

  ! Find the long option by name
  do i = 1, size(longopts)
    if (trim(longopts(i)%name) == name) then
      if (longopts(i)%has_arg) then
        ! Needs a value
        if (eqpos > 0) then
          optarg = trim(arg(eqpos+1:))        ! --opt=value
          if (len_trim(optarg) == 0) then
            optarg = trim(arg)
            c_lng = '?'
            return
          end if
        else
          ! --opt value  (value must be the next token)
          argc = command_argument_count()
          if (optind+1 <= argc) then
            call get_command_argument(optind+1, nextval)
            if (len_trim(nextval) == 0) then
              optarg = trim(arg)
              c_lng = '?'
              return
            end if
            optarg = trim(nextval)
            optind = optind + 1               ! consume the value token
          else
            optarg = trim(arg)
            c_lng = '?'
            return
          end if
        end if
      else
        ! Flag (no value) — must NOT appear as "--flag=value"
        if (eqpos > 0) then
          optarg = trim(arg)                  ! malformed: value given to flag
          c_lng  = '?'
          return
        end if
      end if

      c_lng = longopts(i)%short
      return
    end if
  end do

  ! Not found → unknown long option
  optarg = trim(arg)
  c_lng = '?'
end function process_long

    ! ----------------------------------------
    character function process_short( optstring, arg )
        ! arguments
        character(len=*), intent(in):: optstring, arg

        ! local variables
        integer:: i, arglen

        arglen = len( trim( arg ))
        optopt = arg(grpind:grpind)
        process_short = optopt

        i = index( optstring, optopt )
        if ( i == 0 ) then
            ! unrecognized option
            process_short = '?'
            if ( opterr ) then
                write(stderr, '(a,a,a)') "ERROR: Unrecognized option '-", optopt, "'"
            endif
        endif
        if ( i > 0 .and. substr( optstring, i+1, i+1 ) == ':' ) then
            ! required argument
            optind = optind + 1
            if ( arglen > grpind ) then
                ! -xarg, return remainder of arg
                optarg = arg(grpind+1:arglen)
            elseif ( optind <= command_argument_count()) then
                ! -x arg, return next arg
                call get_command_argument( optind, optarg )
                optind = optind + 1
            elseif ( opterr ) then
                write(stderr, '(a,a,a)') "ERROR: Option '-", optopt, "' requires a value"
                process_short = char(0) ! Option not valid
                stop
            endif
            grpind = 2
        elseif ( arglen > grpind ) then
            ! no argument (or unrecognized), go to next option in argument (-xyz)
            grpind = grpind + 1
        else
            ! no argument (or unrecognized), go to next argument
            grpind = 2
            optind = optind + 1
        endif
    end function process_short

end module f90getopt
