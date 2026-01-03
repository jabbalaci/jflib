module jassert

! Assertions.

   use iso_fortran_env, only: stderr => error_unit
   implicit none
   private

   public :: assert, assert_true, assert_false

contains

   ! call assert(to_str(2025) == "2026", "oops", __FILE__, __LINE__)  ! fail

   subroutine assert(condition, message, file, line)
      !# classic assert, the condition must be true
      logical, intent(in) :: condition
      character(len=*), intent(in), optional :: message
      character(len=*), intent(in), optional :: file
      integer, intent(in), optional :: line
      if (.not. condition) then
         write (stderr, '(*(g0))') "ASSERTION FAILED"
         if (present(message)) write (stderr, '(*(g0))') " Message: ", message
         if (present(file)) write (stderr, '(*(g0))') " File: ", file
         if (present(line)) write (stderr, '(*(g0))') " Line: ", line
         error stop 1
      end if
   end subroutine

   subroutine assert_true(condition, message, file, line)
      !# to avoid writing this: <logical_value> .eqv. .true.
      logical, intent(in) :: condition
      character(len=*), intent(in), optional :: message
      character(len=*), intent(in), optional :: file
      integer, intent(in), optional :: line

      call assert(condition, message, file, line)
   end subroutine

   subroutine assert_false(condition, message, file, line)
      !# to avoid writing this: <logical_value> .eqv. .false.
      logical, intent(in) :: condition
      character(len=*), intent(in), optional :: message
      character(len=*), intent(in), optional :: file
      integer, intent(in), optional :: line

      call assert(.not. condition, message, file, line)
   end subroutine

end module jassert
