program test_jstringbuffer
   use jassert
   use jstringbuffer, only: StringBuffer
   use jconv, only: to_str
   implicit none

   call test_to_string()
   call test_join()
   call test_clear()
   call test_equals()
   call test_get()
   call test_set()
   call test_copy()
   call test_sort()
   call test_sorted()
   call test_is_sorted()
   call test_pop()
   call test_is_empty()

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

   subroutine test_get()
      type(StringBuffer) :: sb

      call sb%append("aa")
      call sb%append("bb")
      call sb%append("cc")
      call assert(sb%get(1) == "aa")
      call assert(sb%get(2) == "bb")
      call assert(sb%get(3) == "cc")
   end subroutine

   subroutine test_set()
      type(StringBuffer) :: sb

      call sb%append("aa"); call sb%append("bb"); call sb%append("cc")
      call sb%set(1, "xx")
      call assert(sb%join(",") == "xx,bb,cc")
      call sb%set(2, "xx")
      call assert(sb%join(",") == "xx,xx,cc")
      call sb%set(3, "xx")
      call assert(sb%join(",") == "xx,xx,xx")
   end subroutine

   subroutine test_sort()
      type(StringBuffer) :: got, expected
      integer :: i

      call assert(got%number_of_elems() == 0)
      call got%sort()
      call assert(got%number_of_elems() == 0)
      call got%append("cc")
      call expected%append("cc")
      call got%sort()
      call assert_true(got%equals(expected))
      call got%append("bb")
      call expected%clear()
      call expected%append("bb"); call expected%append("cc")
      call assert_false(got%equals(expected))
      call got%sort()
      call assert_true(got%equals(expected))
      call got%append("aa")
      call expected%clear()
      call expected%append("aa"); call expected%append("bb"); call expected%append("cc")
      call assert_false(got%equals(expected))
      call got%sort()
      call assert_true(got%equals(expected))
      !#
      call got%clear(); call expected%clear()
      do i = 1,9
         call got%append(to_str(10-i))    !# 9, 8, ..., 1
         call expected%append(to_str(i))  !# 1, 2, ..., 9
      end do
      call assert_false(got%equals(expected))
      call got%sort()
      call assert_true(got%equals(expected))
   end subroutine

   subroutine test_copy()
      type(StringBuffer) :: a, copy

      call a%append("aa"); call a%append("bb"); call a%append("cc")
      copy = a%copy()
      call assert_true(a%equals(copy))
      call a%set(1, "xx")
      call assert(a%join(",") == "xx,bb,cc")
      call assert(copy%join(",") == "aa,bb,cc")
      call assert_false(a%equals(copy))
   end subroutine

   subroutine test_sorted()
      type(StringBuffer) :: original, copy

      call original%append("cc")
      call original%append("bb")
      call original%append("aa")
      call assert(original%join(",") == "cc,bb,aa")
      copy = original%sorted()
      call assert(original%join(",") == "cc,bb,aa")
      call assert(copy%join(",") == "aa,bb,cc")
   end subroutine

   subroutine test_is_sorted()
      type(StringBuffer) :: sb

      call assert_true(sb%is_sorted())
      call sb%append("aa")
      call assert_true(sb%is_sorted())
      call sb%append("bb")
      call assert_true(sb%is_sorted())
      call sb%append("ee")
      call assert_true(sb%is_sorted())
      call sb%append("cc")
      call assert_false(sb%is_sorted())
      !#
      call sb%clear()
      call assert_true(sb%is_sorted())
      call sb%append("ball")
      call assert_true(sb%is_sorted())
      call sb%append("apple")
      call assert_false(sb%is_sorted())
      !#
      call sb%clear()
      call assert_true(sb%is_sorted())
      call sb%append("aa")
      call sb%append("aa")
      call sb%append("aa")
      call assert_true(sb%is_sorted())
   end subroutine

   subroutine test_pop()
      type(StringBuffer) :: sb
      character(len=:), allocatable :: value

      call sb%append("aa")
      call sb%append("bb")
      call sb%append("cc")
      call assert(sb%number_of_elems() == 3)
      value = sb%pop()
      call assert(value == "cc")
      call assert(sb%number_of_elems() == 2)
      value = sb%pop()
      call assert(value == "bb")
      call assert(sb%number_of_elems() == 1)
      value = sb%pop()
      call assert(value == "aa")
      call assert(sb%number_of_elems() == 0)
   end subroutine

   subroutine test_is_empty()
      type(StringBuffer) :: sb
      character(len=:), allocatable :: dummy

      call assert_true(sb%is_empty())
      call sb%append("aa")
      call assert_false(sb%is_empty())
      dummy = sb%pop()
      call assert_true(sb%is_empty())
      call sb%append("aa")
      call assert_false(sb%is_empty())
   end subroutine

end program
