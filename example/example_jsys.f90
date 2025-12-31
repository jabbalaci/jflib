program example_jsys
   use jsys, only: stdin, stdout, stderr, argv
   implicit none
   integer :: i, argc

   print '(*(g0))', "stdin: ", stdin
   print '(*(g0))', "stdout: ", stdout
   print '(*(g0))', "stderr: ", stderr
   print '(a)', "---"
   argc = command_argument_count()
   do i = 0, argc
      print '(*(g0))', i, ": ", argv(i)
   end do
end program
