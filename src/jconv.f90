module jconv

! Conversion functions.

   use jassert
   use jstring, only: lower
   implicit none
   private

   public :: to_str, to_int, to_hex, to_bin

contains

   function to_str(n) result(result)
      !# integer to string (ex.: 25 -> "25")
      integer, intent(in) :: n
      character(len=:), allocatable :: result
      character(len=32) :: buffer

      write (buffer, '(i0)') n
      result = trim(buffer)
   end function

   function to_int(s, iostat) result(result)
      !# string to integer (ex.: "42" -> 42)
      !# If you are not sure of the success of the conversion, call
      !# it with an integer (optional `iostat`), whose value you can check on the caller side.
      character(len=*), intent(in) :: s
      integer, intent(out), optional :: iostat
      integer :: result

      if (present(iostat)) then
         read (s, *, iostat=iostat) result
      else
         read (s, *) result
      end if
   end function

   function to_hex(n) result(result)
      !# integer to hexa (ex.: 2026 -> "0x7ea")
      integer, intent(in) :: n
      character(len=:), allocatable :: result
      character(len=32) :: buffer

      call assert(n >= 0, "Error: provide a non-negative number")

      write (buffer, '(z0)') n
      result = "0x"//lower(trim(buffer))
   end function

   function to_bin(n) result(result)
      !# integer to binary (ex.: 26 -> "0b11010")
      integer, intent(in) :: n
      character(len=:), allocatable :: result
      character(len=32) :: buffer

      call assert(n >= 0, "Error: provide a non-negative number")

      write (buffer, '("0b", b0)') n
      result = trim(buffer)
   end function

end module jconv
