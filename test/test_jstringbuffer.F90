program test_jstringbuffer
   use jassert
   use jstringbuffer, only: StringBuffer
   use jconv, only: to_str
   implicit none

   call test_add_to_set()
   call test_adjust_capacity()
   call test_capacity()
   call test_clear()
   call test_contains()
   call test_copy()
   call test_count_elems()
   call test_del()
   call test_equals()
   call test_find()
   call test_get()
   call test_insert()
   call test_is_empty()
   call test_is_sorted()
   call test_join()
   call test_last()
   call test_max_elem()
   call test_min_elem()
   call test_pop()
   call test_pop_with_index()
   call test_remove()
   call test_reverse()
   call test_reversed()
   call test_rotate()
   call test_set()
   call test_slice()
   call test_slice_negative()
   call test_sort()
   call test_sorted()
   call test_swap()
   call test_to_string()

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
      do i = 1, 9
         call got%append(to_str(10 - i))    !# 9, 8, ..., 1
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

      call sb%append("aa"); call sb%append("bb"); call sb%append("cc")
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

   subroutine test_pop_with_index()
      type(StringBuffer) :: sb
      character(len=:), allocatable :: value

      call sb%append("aa"); call sb%append("bb"); call sb%append("cc")
      call assert(sb%number_of_elems() == 3)
      value = sb%pop(2)
      call assert(value == "bb")
      call assert(sb%number_of_elems() == 2)
      !#
      value = sb%pop(1)
      call assert(value == "aa")
      call assert(sb%number_of_elems() == 1)
      !#
      call sb%append("dd")
      call assert(sb%join(",") == "cc,dd")
      value = sb%pop()
      call assert(value == "dd")
      call assert(sb%number_of_elems() == 1)
      !#
      value = sb%pop(1)
      call assert(value == "cc")
      call assert(sb%number_of_elems() == 0)
      !#
      call sb%append("xx")
      call assert(sb%number_of_elems() == 1)
      value = sb%pop(1)
      call assert(value == "xx")
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

   subroutine test_slice()
      type(StringBuffer) :: sb, got, expected
      integer :: i

      call sb%append("a"); call sb%append("b"); call sb%append("c")
      got = sb%slice(1, 1)
      call expected%append("a")
      call assert_true(got%equals(expected))
      !#
      call expected%clear()
      got = sb%slice(2, 2)
      call expected%append("b")
      call assert_true(got%equals(expected))
      !#
      call expected%clear()
      got = sb%slice(3, 3)
      call expected%append("c")
      call assert_true(got%equals(expected))
      !#
      got = sb%slice()
      call assert_true(sb%equals(got))
      !#
      call sb%clear()
      do i = 1, 9
         call sb%append(to_str(i))
      end do
      !#
      got = sb%slice(1, 3)
      call assert(got%number_of_elems() == 3)
      call assert(got%get(1) == "1" .and. got%get(2) == "2" .and. got%get(3) == "3")
      !#
      got = sb%slice(1, 100)
      call assert_true(sb%equals(got))
      !#
      got = sb%slice(3, 4)
      call assert(got%number_of_elems() == 2)
      call assert(got%get(1) == "3" .and. got%get(2) == "4")
      !#
      got = sb%slice(3, 3)
      call assert(got%number_of_elems() == 1)
      call assert(got%get(1) == "3")
      !#
      got = sb%slice(9, 9)
      call assert(got%number_of_elems() == 1)
      call assert(got%get(1) == "9")
      !#
      got = sb%slice(3, 2)
      call assert_true(got%is_empty())
      !#
      got = sb%slice(20, 30)
      call assert_true(got%is_empty())
      !#
      got = sb%slice(20, 5)
      call assert_true(got%is_empty())
      !#
      got = sb%slice(1, 9, 2)
      call expected%clear()
      call expected%append("1"); call expected%append("3"); call expected%append("5")
      call expected%append("7"); call expected%append("9")
      call assert_true(got%equals(expected))
      !#
      got = sb%slice(1, 5, 2)
      call expected%clear()
      call expected%append("1"); call expected%append("3"); call expected%append("5")
      call assert_true(got%equals(expected))
      !#
      got = sb%slice(1, 6, 2)
      call expected%clear()
      call expected%append("1"); call expected%append("3"); call expected%append("5")
      call assert_true(got%equals(expected))
      !#
      got = sb%slice(1, 9, 1)
      call assert_true(sb%equals(got))
      !#
      got = sb%slice(4, 2, 1)
      call assert_true(got%is_empty())
      !#
      got = sb%slice(4, 4, 1)
      call expected%clear()
      call expected%append("4")
      call assert_true(got%equals(expected))
      !#
      call sb%clear()
      do i = 1, 4
         call sb%append(to_str(i))  !# ["1", "2", "3", "4"]
      end do
      !#
      got = sb%slice(2)
      call expected%clear()
      call expected%append("2"); call expected%append("3"); call expected%append("4")
      call assert_true(got%equals(expected))
      !#
      got = sb%slice(1, 3)
      call expected%clear()
      call expected%append("1"); call expected%append("2"); call expected%append("3")
      call assert_true(got%equals(expected))
   end subroutine

   subroutine test_slice_negative()
      type(StringBuffer) :: sb, got, expected, temp
      integer :: i

      do i = 1, 9
         call sb%append(to_str(i))
      end do
      !#
      got = sb%slice(4, 3, -1)
      call expected%clear()
      call expected%append("4"); call expected%append("3")
      call assert_true(got%equals(expected))
      !#
      got = sb%slice(9, 1, -1)
      expected = sb%reversed()
      call assert_true(got%equals(expected))
      !#
      got = sb%slice(9, 1, -2)
      call expected%clear()
      call expected%append("9"); call expected%append("7"); call expected%append("5")
      call expected%append("3"); call expected%append("1")
      call assert_true(got%equals(expected))
      !#
      got = sb%slice(8, 1, -2)
      call expected%clear()
      call expected%append("8"); call expected%append("6")
      call expected%append("4"); call expected%append("2")
      call assert_true(got%equals(expected))
      !#
      got = sb%slice(5, 1, -1)
      temp = sb%slice(1, 5)
      expected = temp%reversed()
      call assert_true(got%equals(expected))
      !#
      got = sb%slice(20, 1, -1)
      expected = sb%reversed()
      call assert_true(got%equals(expected))
      !#
      got = sb%slice(step=-1)
      expected = sb%reversed()
      call assert_true(got%equals(expected))
   end subroutine

   subroutine test_del()
      type(StringBuffer) :: sb, expected

      call sb%append("aa")
      call sb%del(1)
      call assert_true(sb%is_empty())
      !#
      call sb%append("aa"); call sb%append("bb"); call sb%append("cc")
      call sb%del(1)
      call expected%append("bb"); call expected%append("cc")
      call assert(sb%number_of_elems() == 2)
      call assert_true(sb%equals(expected))
      !#
      call sb%clear(); call expected%clear()
      call sb%append("aa"); call sb%append("bb"); call sb%append("cc")
      call sb%del(2)
      call expected%append("aa"); call expected%append("cc")
      call assert(sb%number_of_elems() == 2)
      call assert_true(sb%equals(expected))
      !#
      call sb%clear(); call expected%clear()
      call sb%append("aa"); call sb%append("bb"); call sb%append("cc")
      call sb%del(3)
      call expected%append("aa"); call expected%append("bb")
      call assert(sb%number_of_elems() == 2)
      call assert_true(sb%equals(expected))
   end subroutine

   subroutine test_insert()
      type(StringBuffer) :: sb

      call sb%clear()
      call sb%append("aa"); call sb%append("bb"); call sb%append("cc")
      call sb%insert(1, "xx")
      call assert(sb%join(",") == "xx,aa,bb,cc")
      !#
      call sb%clear()
      call sb%append("aa"); call sb%append("bb"); call sb%append("cc")
      call sb%insert(2, "xx")
      call assert(sb%join(",") == "aa,xx,bb,cc")
      !#
      call sb%clear()
      call sb%append("aa"); call sb%append("bb"); call sb%append("cc")
      call sb%insert(3, "xx")
      call assert(sb%join(",") == "aa,bb,xx,cc")
   end subroutine

   subroutine test_find()
      type(StringBuffer) :: sb

      call sb%append("aa"); call sb%append("bb"); call sb%append("cc"); call sb%append("aa")
      call assert(sb%find("aa") == 1)
      call assert(sb%find("bb") == 2)
      call assert(sb%find("cc") == 3)
      call assert(sb%find("xx") == 0)
   end subroutine

   subroutine test_remove()
      type(StringBuffer) :: sb
      logical :: status

      status = sb%remove("xx")
      call assert_false(status)
      !#
      call sb%append("aa"); call sb%append("bb"); call sb%append("cc"); call sb%append("aa")
      call assert(sb%join(",") == "aa,bb,cc,aa")
      status = sb%remove("aa")
      call assert_true(status)
      call assert(sb%join(",") == "bb,cc,aa")
      !#
      status = sb%remove("aa")
      call assert_true(status)
      call assert(sb%join(",") == "bb,cc")
      !#
      status = sb%remove("aa")
      call assert_false(status)
      call assert(sb%join(",") == "bb,cc")
      !#
      status = sb%remove("bb")
      call assert_true(status)
      call assert(sb%join(",") == "cc")
      !#
      status = sb%remove("cc")
      call assert_true(status)
      call assert(sb%join(",") == "")
      call assert_true(sb%is_empty())
   end subroutine

end program
