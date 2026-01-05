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
   call test_contains()
   call test_add_to_set()
   call test_capacity()
   call test_last()
   call test_min_elem()
   call test_max_elem()
   call test_count_elems()
   call test_swap()
   call test_reverse()
   call test_reversed()
   call test_rotate()
   call test_adjust_capacity()

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

   subroutine test_contains()
      type(StringBuffer) :: sb

      call assert_false(sb%contains("aa"))
      call sb%append("aa")
      call assert_true(sb%contains("aa"))
      call sb%append("bb"); call sb%append("cc"); call sb%append("dd")
      call assert_true(sb%contains("aa"))
      call assert_true(sb%contains("bb"))
      call assert_true(sb%contains("cc"))
      call assert_true(sb%contains("dd"))
      call assert_false(sb%contains("xx"))
   end subroutine

   subroutine test_add_to_set()
      type(StringBuffer) :: sb

      call assert(sb%number_of_elems() == 0)
      call sb%add_to_set("aa")
      call assert(sb%number_of_elems() == 1)
      call sb%add_to_set("aa")
      call sb%add_to_set("aa")
      call sb%add_to_set("aa")
      call assert(sb%number_of_elems() == 1)
      call sb%add_to_set("bb")
      call assert(sb%number_of_elems() == 2)
      call sb%add_to_set("bb")
      call sb%add_to_set("bb")
      call sb%add_to_set("bb")
      call sb%add_to_set("bb")
      call assert(sb%number_of_elems() == 2)
   end subroutine

   subroutine test_capacity()
      type(StringBuffer) :: sb, big
      integer :: old_capacity, new_capacity, i

      old_capacity = sb%get_capacity()
      call assert(sb%number_of_elems() == 0)
      call assert(sb%get_capacity() == old_capacity)
      call sb%set_capacity(old_capacity - 1)  !# smaller
      call assert(sb%get_capacity() == old_capacity)
      new_capacity = old_capacity + 1
      call sb%set_capacity(new_capacity)
      call assert(sb%get_capacity() == new_capacity)
      call sb%clear()
      call assert(sb%get_capacity() == old_capacity)
      !#
      old_capacity = big%get_capacity()
      call assert(big%get_capacity() == old_capacity)
      call big%set_capacity(20)
      call assert(big%get_capacity() == 20)
      do i = 1, 100
         call big%append(to_str(i))
      end do
      call assert(big%number_of_elems() == 100)
      call assert(big%get_capacity() >= 100)
      call big%set_capacity(10000)
      call assert(big%number_of_elems() == 100)
      call assert(big%get_capacity() == 10000)
   end subroutine

   subroutine test_last()
      type(StringBuffer) :: sb
      character(len=:), allocatable :: dummy

      call sb%append("aa")
      call assert(sb%last() == "aa")
      call sb%append("bb")
      call assert(sb%last() == "bb")
      dummy = sb%pop()
      call assert(sb%last() == "aa")
   end subroutine

   subroutine test_min_elem()
      type(StringBuffer) :: sb
      integer :: i, numbers(5)

      call sb%append("bb")
      call assert(sb%min_elem() == "bb")
      call sb%append("aa")
      call assert(sb%min_elem() == "aa")
      call sb%append("cc")
      call assert(sb%min_elem() == "aa")
      !#
      call sb%clear()
      numbers = [5, 4, 1, 3, 2]
      do i = 1, size(numbers)
         call sb%append(to_str(numbers(i)))
      end do
      call assert(sb%join(",") == "5,4,1,3,2")
      call assert(sb%min_elem() == "1")
   end subroutine

   subroutine test_max_elem()
      type(StringBuffer) :: sb
      integer :: i, numbers(5)

      call sb%append("bb")
      call assert(sb%max_elem() == "bb")
      call sb%append("aa")
      call assert(sb%max_elem() == "bb")
      call sb%append("cc")
      call assert(sb%max_elem() == "cc")
      !#
      call sb%clear()
      numbers = [1, 4, 5, 3, 2]
      do i = 1, size(numbers)
         call sb%append(to_str(numbers(i)))
      end do
      call assert(sb%join(",") == "1,4,5,3,2")
      call assert(sb%max_elem() == "5")
   end subroutine

   subroutine test_count_elems()
      type(StringBuffer) :: sb

      call sb%append("aa")
      call sb%append("bb"); call sb%append("bb"); call sb%append("bb")
      call sb%append("cc"); call sb%append("cc")

      call assert(sb%count_elems("xx") == 0)
      call assert(sb%count_elems("aa") == 1)
      call assert(sb%count_elems("bb") == 3)
      call assert(sb%count_elems("cc") == 2)
   end subroutine

   subroutine test_swap()
      type(StringBuffer) :: sb

      call sb%append("aa"); call sb%append("bb"); call sb%append("cc")
      call assert(sb%join(",") == "aa,bb,cc")
      call sb%swap(1, 3)
      call assert(sb%join(",") == "cc,bb,aa")
      call sb%swap(1, 1)
      call assert(sb%join(",") == "cc,bb,aa")
      call sb%swap(2, 3)
      call assert(sb%join(",") == "cc,aa,bb")
      call sb%swap(3, 2)
      call assert(sb%join(",") == "cc,bb,aa")
   end subroutine

   subroutine test_reverse()
      type(StringBuffer) :: got

      call got%reverse()
      call assert(got%is_empty())
      !#
      call got%append("aa")
      call got%reverse()
      call assert(got%number_of_elems() == 1 .and. got%get(1) == "aa")
      !#
      call got%append("bb")
      call assert(got%get(1) == "aa" .and. got%get(2) == "bb")
      call got%reverse()
      call assert(got%get(1) == "bb" .and. got%get(2) == "aa")
      call got%reverse()
      call assert(got%get(1) == "aa" .and. got%get(2) == "bb")
      !#
      call got%append("cc")
      call assert(got%last() == "cc")
      call assert(got%get(1) == "aa" .and. got%get(2) == "bb" .and. got%get(3) == "cc")
      call got%reverse()
      call assert(got%get(1) == "cc" .and. got%get(2) == "bb" .and. got%get(3) == "aa")
      call got%reverse()
      call assert(got%get(1) == "aa" .and. got%get(2) == "bb" .and. got%get(3) == "cc")
   end subroutine

   subroutine test_reversed()
      type(StringBuffer) :: original, reversed

      call original%append("aa"); call original%append("bb"); call original%append("cc")
      call assert(original%join(",") == "aa,bb,cc")
      reversed = original%reversed()
      call assert(original%join(",") == "aa,bb,cc")
      call assert(reversed%join(",") == "cc,bb,aa")
   end subroutine

   subroutine test_rotate()
      type(StringBuffer) :: sb

      call sb%rotate(0)
      call sb%append("a"); call sb%append("b"); call sb%append("c")
      call assert(sb%join(",") == "a,b,c")
      call sb%rotate(0)
      call assert(sb%join(",") == "a,b,c")
      call sb%rotate(3)
      call assert(sb%join(",") == "a,b,c")
      call sb%rotate(6)
      call assert(sb%join(",") == "a,b,c")
      call sb%rotate(-3)
      call assert(sb%join(",") == "a,b,c")
      call sb%rotate(-6)
      call assert(sb%join(",") == "a,b,c")
      !#
      call assert(sb%join(",") == "a,b,c")
      call sb%rotate(1)
      call assert(sb%join(",") == "c,a,b")
      call sb%rotate(1)
      call assert(sb%join(",") == "b,c,a")
      call sb%rotate(1)
      call assert(sb%join(",") == "a,b,c")
      call sb%rotate(2026)
      call assert(sb%join(",") == "c,a,b")
      !#
      call assert(sb%join(",") == "c,a,b")
      call sb%rotate(-1)
      call assert(sb%join(",") == "a,b,c")
      call sb%rotate(-1)
      call assert(sb%join(",") == "b,c,a")
      call sb%rotate(-1)
      call assert(sb%join(",") == "c,a,b")
      call sb%rotate(-1)
      call assert(sb%join(",") == "a,b,c")
      call sb%rotate(-2026)
      call assert(sb%join(",") == "b,c,a")
      !#
      call sb%rotate(1)
      call assert(sb%join(",") == "a,b,c")
      call sb%append("d")
   end subroutine

   subroutine test_adjust_capacity()
      type(StringBuffer) :: sb

      call assert(sb%number_of_elems() == 0)
      call assert(sb%get_capacity() > 0)
      call assert(sb%get_array_size() == 0)
      !#
      call sb%append("a")
      call assert(sb%number_of_elems() == 1)
      call assert(sb%get_capacity() > 0)
      call assert(sb%get_array_size() == sb%get_capacity())
      !#
      call sb%append("b")
      call sb%append("c")
      call assert(sb%number_of_elems() == 3)
      call assert(sb%get_array_size() == sb%get_capacity())
      !#
      call sb%rotate(1)  !# it'll shrink the underlying array
      call assert(sb%number_of_elems() == 3)
      call assert(sb%get_capacity() == 3)
      call assert(sb%get_array_size() == 3)
      !#
      call sb%append("d")
      call assert(sb%number_of_elems() == 4)
      call assert(sb%get_capacity() >= 4)
      call assert(sb%get_array_size() == sb%get_capacity())
   end subroutine

end program
