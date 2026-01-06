program test_jconv
   use jassert, only: assert
   use jconv
   implicit none

   call test_to_bin()
   call test_to_hex()
   call test_to_str()

   print '(a)', "OK"
contains

   subroutine test_to_str()
      call assert(to_str(2025) == "2025")
      call assert(to_str(42) == "42")
      call assert(to_str(1) == "1")
      call assert(to_str(0) == "0")
      call assert(to_str(-5) == "-5")
      call assert(to_str(-2000) == "-2000")
   end subroutine

   subroutine test_to_hex()
      call assert(to_hex(2026) == "0x7ea")
      call assert(to_hex(0) == "0x0")
      call assert(to_hex(42) == "0x2a")
      call assert(to_hex(1) == "0x1")
   end subroutine

   subroutine test_to_bin()
      call assert(to_bin(2026) == "0b11111101010")
      call assert(to_bin(0) == "0b0")
      call assert(to_bin(42) == "0b101010")
      call assert(to_bin(1) == "0b1")
   end subroutine

end program
