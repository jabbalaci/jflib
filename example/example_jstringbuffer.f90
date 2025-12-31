program example_jstringbuffer
   use jstringbuffer, only: StringBuffer
   implicit none

   type(StringBuffer) :: sb

   call sb%append("a")
   call sb%append("ccc")
   call sb%append("bb")

   call sb%debug()
   print '(i0)', sb%total_length()
   print '(*(g0))', "'", sb%to_string(), "'"
   print '(*(g0))', "'", sb%join(", "), "'"
end program
