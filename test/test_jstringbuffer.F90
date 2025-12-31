program test_jstringbuffer
   use jassert
   use jstringbuffer, only: StringBuffer
   use jconv, only: to_str
   implicit none

   call test_to_string()
   call test_join()
   call test_clear()
   call test_equals()

   print '(a)', "OK"

contains

   subroutine test_to_string()
      type(StringBuffer) :: sb
      call assert(sb%to_string() == "")
      call sb%append("a")
      call assert(sb%to_string() == "a")
      call sb%append("ccc")
      call assert(sb%to_string() == "accc")
      call sb%append("bb")
      call assert(sb%to_string() == "acccbb")
      call sb%append(to_str(90))
      call assert(sb%to_string() == "acccbb90")
   end subroutine

   subroutine test_join()
      type(StringBuffer) :: sb
      call assert(sb%join() == "")
      call assert(sb%join("") == "")
      call assert(sb%join(",") == "")
      call assert(sb%join(", ") == "")
      call assert(sb%join("---") == "")
      call sb%append("a")
      call assert(sb%join() == "a")
      call assert(sb%join("") == "a")
      call assert(sb%join(",") == "a")
      call assert(sb%join(", ") == "a")
      call assert(sb%join("---") == "a")
      call sb%append("ccc")
      call assert(sb%join() == "accc")
      call assert(sb%join("") == "accc")
      call assert(sb%join(",") == "a,ccc")
      call assert(sb%join(", ") == "a, ccc")
      call assert(sb%join("---") == "a---ccc")
      call sb%append("bb")
      call assert(sb%join() == "acccbb")
      call assert(sb%join("") == "acccbb")
      call assert(sb%join(",") == "a,ccc,bb")
      call assert(sb%join(", ") == "a, ccc, bb")
      call assert(sb%join("---") == "a---ccc---bb")
      call sb%append(to_str(90))
      call assert(sb%join() == "acccbb90")
      call assert(sb%join("") == "acccbb90")
      call assert(sb%join(",") == "a,ccc,bb,90")
      call assert(sb%join(", ") == "a, ccc, bb, 90")
      call assert(sb%join("---") == "a---ccc---bb---90")
   end subroutine

   subroutine test_clear()
      type(StringBuffer) :: sb

      call sb%clear()
      call assert(sb%to_string() == "")
      call assert(sb%number_of_elems() == 0)
      !#
      call sb%append("aa")
      call assert(sb%to_string() == "aa")
      call assert(sb%number_of_elems() == 1)
      !#
      call sb%clear()
      call assert(sb%to_string() == "")
      call assert(sb%number_of_elems() == 0)
      !#
      call sb%append("aa"); call sb%append("bb"); call sb%append("cc")
      call assert(sb%to_string() == "aabbcc")
      call assert(sb%number_of_elems() == 3)
      !#
      call sb%clear()
      call assert(sb%to_string() == "")
      call assert(sb%number_of_elems() == 0)
   end subroutine

   subroutine test_equals()
      type(StringBuffer) :: a
      type(StringBuffer) :: b

      call assert_true(a%equals(b))
      call b%append("xxx")
      call assert_false(a%equals(b))
      call a%append("xxx")
      call assert_true(a%equals(b))
      call a%append("yyy")
      call assert_false(a%equals(b))
      call b%append("yyy")
      call assert_true(a%equals(b))
      call a%clear()
      call assert_false(a%equals(b))
      call b%clear()
      call assert_true(a%equals(b))
   end subroutine

end program
