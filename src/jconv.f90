module jconv

! Conversion functions.

   implicit none
   private

   public :: to_str

contains

   function to_str(n) result(result)
      !# integer to string (ex.: 25 -> "25")
      integer, intent(in) :: n
      character(len=:), allocatable :: result
      character(len=20) :: buffer

      write (buffer, '(i0)') n
      result = trim(adjustl(buffer))
   end function

end module jconv
