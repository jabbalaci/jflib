program example_jtypes
   use jtypes
   implicit none
   real(sp) :: x = 2.55_sp
   real(dp) :: pi = 3.14159_dp

   print *, x, pi
   print '(a)', "---"
end program
