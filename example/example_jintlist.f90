program example_jintlist
   use jintlist, only: IntList
   implicit none
   type(IntList) :: li
   integer :: i

   do i = 1, 10
      call li%append(i)
   end do

   call li%debug()
   print '(i0)', li%number_of_elems()
   print '(*(g0,x))', li%to_array()
end program
