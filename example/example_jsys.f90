program example_jsys
   use iso_fortran_env, only: stdin => input_unit, &    !# 5
                              stdout => output_unit, &  !# 6
                              stderr => error_unit      !# 0
   use jsys, only: argc, argv
   implicit none
   integer :: i

   print '(*(g0))', "stdin: ", stdin
   print '(*(g0))', "stdout: ", stdout
   print '(*(g0))', "stderr: ", stderr
   print '(a)', "---"
   do i = 0, argc()
      print '(*(g0))', i, ": ", argv(i)
   end do
   print '(a)', "---"
end program
