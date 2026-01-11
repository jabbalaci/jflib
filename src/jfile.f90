module jfile

! File handling.

   use iso_fortran_env, only: stderr => error_unit
   use jstringbuffer, only: StringBuffer
   implicit none
   private

   character, parameter :: LF = achar(10)    !# '\n'

   public :: readlines, read_content

contains

   function readlines(fname, iostat) result(result)
      !# Read the content of a text file and return the lines in a stringbuffer.
      !# Newline characters are NOT included at the end of the lines.
      !# With `iostat` you can check if the operation was successful (value 0) or not.
      character(len=*), intent(in) :: fname
      integer, intent(out) :: iostat
      type(StringBuffer) :: result
      integer :: unit, ios
      character(len=1024) :: buffer
      character(len=:), allocatable :: line

      open (newunit=unit, file=fname, status="old", action="read", iostat=iostat)
      if (iostat /= 0) then
         return
      end if
      !# if it could be opened for read
      do
         read (unit, '(a)', iostat=ios) buffer
         if (ios /= 0) then
            exit  !# break
         end if
         line = trim(buffer)
         if (len(line) == len(buffer)) then
            write (stderr, '(*(g0))') "Warning: the buffer is full"
            write (stderr, '(*(g0))') "Tip: increase the size of the buffer in ", __FILE__
         end if
         call result%append(line)
      end do
   end function

   function read_content(fname, iostat) result(result)
      !# Read the content of a text file and return it as a string.
      !# With `iostat` you can check if the operation was successful (value 0) or not.
      character(len=*), intent(in) :: fname
      integer, intent(out) :: iostat
      character(len=:), allocatable :: result
      type(StringBuffer) :: lines

      lines = readlines(fname, iostat)
      if (iostat /= 0) then
         return
      end if
      !# if it could be opened for read
      result = lines%join(LF)
   end function

end module jfile
