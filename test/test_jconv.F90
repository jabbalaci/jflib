program test_jconv
   use jassert, only: assert
   use jconv, only: to_str
   implicit none

   call assert(to_str(2025) == "2025")
   call assert(to_str(42) == "42")
   call assert(to_str(1) == "1")
   call assert(to_str(0) == "0")
   call assert(to_str(-5) == "-5")
   call assert(to_str(-2000) == "-2000")

   print '(a)', "OK"
end program
