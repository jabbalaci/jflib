program example_jsys
   use jsys, only: stdin, stdout, stderr, argc, argv
   implicit none
   integer :: i

   print '(*(g0))', "stdin: ", stdin
   print '(*(g0))', "stdout: ", stdout
   print '(*(g0))', "stderr: ", stderr
   print '(a)', "---"
   do i = 0, argc()
      print '(*(g0))', i, ": ", argv(i)
   end do
end program
