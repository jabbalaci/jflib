program example_jconv
   use jconv
   implicit none
   integer :: iostat

   print '(*(g0))', "to_str(2026): ", '"', to_str(2026), '"'
   print '(*(g0))', "to_hex(2026): ", '"', to_hex(2026), '"'
   print '(*(g0))', "to_bin(26): ", '"', to_bin(26), '"'
   print '(*(g0))', 'to_int("42"): ', to_int("42")
   print '(*(g0))', 'to_int("42-invalid", iostat): ', to_int("42-invalid", iostat)
   if (iostat /= 0) then
      print '(x,*(g0))', 'iostat: ', iostat
      print *, "oops, there was a conversion error here"
   end if
   print '(a)', "---"
end program
