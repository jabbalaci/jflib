module jstringbuffer

! A stringbuffer for storing variable-length strings.

   use iso_fortran_env, only: stderr => error_unit
   use jassert, only: assert
   implicit none
   private
   integer, parameter :: INITIAL_CAPACITY = 8

   public :: StringBuffer

   type :: String
      !# wrapper type (box) for variable-length strings
      character(len=:), allocatable :: s
   end type String

   type :: StringBuffer
      integer, private :: size = 0  ! number of elems
      integer, private :: capacity = INITIAL_CAPACITY  ! initial capacity
      type(String), private, allocatable :: data(:)
   contains
      procedure, private :: quicksort, adjust_capacity
      procedure, public :: &
         add_to_set, &              !# treat it as if it were a set (no duplicates)
         append, &                  !# add an element to the end
         clear, &                   !# reset
         contains, &                !# check if an element is in the stringbuffer
         copy, &                    !# make a complete, independent copy
         count_elems, &             !# a given element is present how many times
         debug, &                   !# print content to stderr
         del, &                     !# delete the element at position `i`
         equals, &                  !# compare with another stringbuffer
         find, &                    !# find the position of an element
         get, &                     !# get the i^{th} element
         get_array_size, &          !# size of the underlying array (for debug purposes)
         get_capacity, &            !# current capacity
         insert, &                  !# insert right before the given position
         is_empty, &                !# Is the stringbuffer empty?
         is_sorted, &               !# Is the stringbuffer sorted?
         join, &                    !# join the elements by a delimiter (default delimiter: "")
         last, &                    !# last elem; first elem: get(1)
         max_elem, &                !# get the largest element
         min_elem, &                !# get the smallest element
         number_of_elems, &         !# number of appended elements
         pop, &                     !# take out and return the i^{th} element
         remove, &                  !# remove a given element
         reverse, &                 !# reverse the elements in-place
         reversed, &                !# return a reversed copy
         rotate, &                  !# rotate the elements to the left/right
         set, &                     !# set the value of the i^{th} element
         set_capacity, &            !# increase the capacity
         slice, &                   !# sb%slice(start, end, step)
         sort, &                    !# sort in-place
         sorted, &                  !# return a sorted copy
         stats, &                   !# some statistics (for debug purposes)
         swap, &                    !# swap two elements
         to_string, &               !# get the content as a string
         total_length               !# to_string() would return a string of this length
   end type StringBuffer

contains

   subroutine clear(self)
      !# Resets the stringbuffer to its initial state.
      !# This way, you can reuse the same object.
      class(StringBuffer), intent(inout) :: self
      self%size = 0
      self%capacity = INITIAL_CAPACITY
      if (allocated(self%data)) then
         deallocate (self%data)
      end if
   end subroutine

   subroutine adjust_capacity(self)  !# private
      !# It can happen that the array (self%data) shrinks (remove()) or grows (insert()),
      !# and thus the array size becomes different from self%capacity.
      !# Solution: adjust self%capacity to the current size of the array.
      class(StringBuffer), intent(inout) :: self

      self%capacity = size(self%data)

      if (self%size == 0) then
         call self%clear()  !# reset the stringbuffer
      end if
   end subroutine

   pure function total_length(self) result(result)
      !# Sum of the length of every element in the stringbuffer.
      !# If we call to_string(), we get a string of this length.
      class(StringBuffer), intent(in) :: self
      integer :: i, result

      result = 0
      do i = 1, self%size
         result = result + len(self%data(i)%s)
      end do
   end function

   pure function number_of_elems(self) result(result)
      !# Number of elements (boxes) in the stringbuffer.
      !# This is NOT equivalent to the total length!
      class(StringBuffer), intent(in) :: self
      integer :: result
      result = self%size
   end function

   pure function get_capacity(self) result(result)
      class(StringBuffer), intent(in) :: self
      integer :: result

      result = self%capacity
   end function

   subroutine set_capacity(self, new_capacity)
      !# Increase the capacity of the stringbuffer.
      !# For advanced usage. If you know that you want to store 1 million elements,
      !# then you can set a high capacity before the first append.
      !# With this, you can only increase the capacity. If you want to set a lower
      !# value than the current capacity, then nothing will happen.
      class(StringBuffer), intent(inout) :: self
      integer, intent(in) :: new_capacity

      if (new_capacity > self%capacity) then
         self%capacity = new_capacity
      end if
   end subroutine

   pure function is_empty(self) result(result)
      class(StringBuffer), intent(in) :: self
      logical :: result

      result = (self%size == 0)
   end function

   pure subroutine append(self, value)
      !# Append a string (in boxed format) to the end.
      class(StringBuffer), intent(inout) :: self
      character(len=*), intent(in) :: value
      type(String), allocatable :: temp(:)

      if (self%size == 0) then
         allocate (self%data(self%capacity))
      end if
      self%size = self%size + 1
      if (self%size > self%capacity) then
         self%capacity = self%capacity * 2
         allocate (temp(self%capacity))
         temp(1:self%size - 1) = self%data(1:self%size - 1)
         call move_alloc(temp, self%data)
      end if
      self%data(self%size) = String(value)
   end subroutine

   subroutine debug(self)
      !# Print the content to the stdout. For debug purposes.
      class(StringBuffer), intent(in) :: self
      integer :: i

      print '(a)', "debug:"
      if (self%size == 0) then
         print *, "(empty)"
      else
         do i = 1, self%size
            print '(x,*(g0))', i, ": ", "'", self%data(i)%s, "'"
         end do
      end if
   end subroutine

   subroutine stats(self)
      !# Print some statistics about the stringbuffer. For debug purposes.
      class(StringBuffer), intent(in) :: self

      print '(a)', "stats:"
      print '(*(g0))', " number of elems: ", self%size
      print '(*(g0))', " capacity: ", self%capacity
      print '(*(g0))', " size of the underlying array: ", self%get_array_size()
      print '(*(g0))', " total length of inserted strings: ", self%total_length()
   end subroutine

   pure function get_array_size(self) result(result)
      !# Returns the size of the underlying array (that contains the elements).
      class(StringBuffer), intent(in) :: self
      integer :: result

      if (.not. allocated(self%data)) then
         result = 0
      else
         result = size(self%data)
      end if
   end function

   function get(self, i) result(result)
      !# Returns the string at position `i`
      class(StringBuffer), intent(in) :: self
      integer, intent(in) :: i
      character(len=:), allocatable :: result

      call assert((1 <= i) .and. (i <= self%size), "IndexError: index out of range")

      result = self%data(i)%s
   end function

   subroutine set(self, i, value)
      !# Change the value of the i^{th} element to `value`.
      class(StringBuffer), intent(inout) :: self
      integer, intent(in) :: i
      character(len=*), intent(in) :: value

      call assert((1 <= i) .and. (i <= self%size), "IndexError: stringbuffer index out of range")

      self%data(i)%s = value
   end subroutine

   pure function to_string(self) result(result)
      !# Get the content of the stringbuffer as a string.
      class(StringBuffer), intent(in) :: self
      character(len=:), allocatable :: result

      result = self%join()
   end function

   ! function to_string(self) result(result)
   !    !# Get the content of the stringbuffer as a string.
   !    class(StringBuffer), intent(in) :: self
   !    character(len=:), allocatable :: result
   !    integer :: i, j, length

   !    if (.not. allocated(self%data)) then
   !       result = ""
   !    else
   !       allocate (character(len=self%total_length()) :: result)
   !       j = 1
   !       do i = 1, self%size
   !          associate (s => self%data(i)%s)
   !             length = len(s)
   !             result(j:j + length - 1) = s
   !             j = j + length
   !          end associate
   !       end do
   !    end if
   ! end function

   pure function join(self, sep) result(result)
      !# Join the elements by a delimiter.
      !# The result is a string.
      class(StringBuffer), intent(in) :: self
      character(len=*), intent(in), optional :: sep
      character(len=:), allocatable :: result
      integer :: i, j, length, extra, len_alloc
      character(len=:), allocatable :: sep_val

      sep_val = ""  !# default value
      if (present(sep)) sep_val = sep

      if (.not. allocated(self%data)) then
         result = ""
      else
         extra = max(0, self%size - 1) * len(sep_val)
         len_alloc = self%total_length() + extra
         ! allocate (character(len=self%total_length() + extra) :: result)  ! compiler error
         allocate (character(len=len_alloc) :: result)  ! it's OK
         j = 1
         do i = 1, self%size
            associate (s => self%data(i)%s)
               if (i > 1) then
                  length = len(sep_val)
                  result(j:j + length - 1) = sep_val
                  j = j + length
               end if
               length = len(s)
               result(j:j + length - 1) = s
               j = j + length
            end associate
         end do
      end if
   end function

   pure function equals(self, other) result(result)
      !# Check if two stringbuffers have the same content.
      !# Returns true or false.
      class(StringBuffer), intent(in) :: self
      type(StringBuffer), intent(in) :: other
      logical :: result
      integer :: i

      if (self%size /= other%size) then
         result = .false.
         return
      end if
      !# else
      do i = 1, self%size
         if (self%data(i)%s /= other%data(i)%s) then
            result = .false.
            return
         end if
      end do
      result = .true.
   end function

   recursive subroutine quicksort(self, left, right)  !# private
      !# Sorts the underlying array in-place with quicksort.
      class(StringBuffer), intent(inout) :: self
      integer, intent(in) :: left, right
      integer :: i, j, pivot_idx
      type(String) :: x, temp

      if (left >= right) return  !# nothing to sort

      i = left
      j = right
      pivot_idx = (left + right) / 2
      x = self%data(pivot_idx)

      do while (i <= j)
         do while (self%data(i)%s < x%s)
            i = i + 1
         end do
         do while (self%data(j)%s > x%s)
            j = j - 1
         end do
         if (i <= j) then
            !# swap elements
            temp = self%data(i)
            self%data(i) = self%data(j)
            self%data(j) = temp
            i = i + 1
            j = j - 1
         end if
      end do

      if (left < j) call self%quicksort(left, j)
      if (i < right) call self%quicksort(i, right)
   end subroutine

   subroutine sort(self)
      !# sort the underlying array in-place
      class(StringBuffer), intent(inout) :: self
      if (self%size == 0) then
         return
      end if
      !# else
      call self%quicksort(1, self%size)
   end subroutine

   function sorted(self) result(result)
      !# Returns a sorted copy of itself. The stringbuffer (self) is NOT modified.
      class(StringBuffer), intent(in) :: self
      type(StringBuffer) :: result

      result = self%copy()
      call result%sort()
   end function

   subroutine reverse(self)
      !# Reverses the underlying array in-place.
      class(StringBuffer), intent(inout) :: self
      integer :: i, j
      type(String) :: temp

      i = 1
      j = self%size
      do while (i < j)
         temp = self%data(i)
         self%data(i) = self%data(j)
         self%data(j) = temp
         !#
         i = i + 1
         j = j - 1
      end do
   end subroutine

   function reversed(self) result(result)
      !# Returns a reversed copy of itself. The stringbuffer (self) is NOT modified.
      class(StringBuffer), intent(in) :: self
      type(StringBuffer) :: result

      result = self%copy()
      call result%reverse()
   end function

   pure function copy(self) result(result)
      !# Creates and returns an independent copy of itself.
      class(StringBuffer), intent(in) :: self
      type(StringBuffer) :: result

      result%size = self%size
      result%capacity = self%capacity
      result%data = self%data
   end function

   pure function is_sorted(self) result(result)
      !# Checks if the content of the stringbuffer is sorted or not.
      !# Returns true or false.
      class(StringBuffer), intent(in) :: self
      logical :: result
      integer :: i

      if (self%size < 2) then
         result = .true.
         return
      end if
      !# else
      do i = 2, self%size
         if (self%data(i - 1)%s > self%data(i)%s) then
            result = .false.
            return
         end if
      end do
      result = .true.
   end function

   function pop(self, i) result(result)
      !# Take out (remove) and return the i^{th} element of the stringbuffer.
      !# If `i` is not provided, then it defaults to the last element.
      class(StringBuffer), intent(inout) :: self
      integer, intent(in), optional :: i
      character(len=:), allocatable :: result
      integer :: i_val, last_idx

      last_idx = self%size
      i_val = last_idx  !# default value
      if (present(i)) i_val = i

      call assert((1 <= i_val) .and. (i_val <= self%size), "IndexError: index out of range")

      result = self%data(i_val)%s
      call self%del(i_val)
   end function

   subroutine del(self, i)
      !# Delete the element at position `i`
      class(StringBuffer), intent(inout) :: self
      integer, intent(in) :: i

      call assert((1 <= i) .and. (i <= self%size), "IndexError: index out of range")

      self%data = [self%data(1:i - 1), self%data(i + 1:self%size)]
      self%size = self%size - 1
      call self%adjust_capacity()  ! Must be called! self%data has changed
   end subroutine

   subroutine insert(self, i, value)
      !# Insert `value` right before the `i` position.
      class(StringBuffer), intent(inout) :: self
      integer, intent(in) :: i
      character(len=*), intent(in) :: value

      call assert((1 <= i) .and. (i <= self%size), "IndexError: index out of range")

      self%data = [self%data(1:i - 1), String(value), self%data(i:self%size)]
      self%size = self%size + 1
      call self%adjust_capacity()  ! Must be called! self%data has changed
   end subroutine

   pure function contains(self, value) result(result)
      !# Checks if `value` is in the stringbuffer or not.
      class(StringBuffer), intent(in) :: self
      character(len=*), intent(in) :: value
      logical :: result
      integer :: i

      do i = 1, self%size
         if (self%data(i)%s == value) then
            result = .true.
            return
         end if
      end do
      result = .false.
   end function

   subroutine add_to_set(self, value)
      !# Treat the stringbuffer as if it were a set.
      !# Add `value` only if it's not yet in the stringbuffer.
      class(StringBuffer), intent(inout) :: self
      character(len=*), intent(in) :: value

      if (.not. self%contains(value)) then
         call self%append(value)
      end if
   end subroutine

   function last(self) result(result)
      !# Returns the last element in the stringbuffer.
      class(StringBuffer), intent(in) :: self
      character(len=:), allocatable :: result

      call assert(self%size > 0, "IndexError: the stringbuffer is empty")

      result = self%data(self%size)%s
   end function

   function min_elem(self) result(result)
      !# Returns the minimal element.
      class(StringBuffer), intent(in) :: self
      character(len=:), allocatable :: result
      integer :: i

      call assert(self%size > 0, "IndexError: the stringbuffer is empty")

      result = self%data(1)%s
      do i = 2, self%size
         if (self%data(i)%s < result) then
            result = self%data(i)%s
         end if
      end do
   end function

   function max_elem(self) result(result)
      !# Returns the maximal element.
      class(StringBuffer), intent(in) :: self
      character(len=:), allocatable :: result
      integer :: i

      call assert(self%size > 0, "IndexError: the stringbuffer is empty")

      result = self%data(1)%s
      do i = 2, self%size
         if (self%data(i)%s > result) then
            result = self%data(i)%s
         end if
      end do
   end function

   pure function count_elems(self, value) result(result)
      !# Counts how many times `value` is present in the stringbuffer.
      class(StringBuffer), intent(in) :: self
      character(len=*), intent(in) :: value
      integer :: i, result

      result = 0
      do i = 1, self%size
         if (self%data(i)%s == value) then
            result = result + 1
         end if
      end do
   end function

   subroutine swap(self, i, j)
      !# Swaps the i^{th} and j^{th} elements.
      class(StringBuffer), intent(inout) :: self
      integer, intent(in) :: i, j
      type(String) :: temp

      call assert((1 <= i) .and. (i <= self%size), "IndexError: index out of range")
      call assert((1 <= j) .and. (j <= self%size), "IndexError: index out of range")

      if (i == j) return
      !# else
      temp = self%data(i)
      self%data(i) = self%data(j)
      self%data(j) = temp
   end subroutine

   subroutine rotate(self, rotations)
      !# Rotates the elements to the left/right by `n` positions.
      !# Positive n: rotate right (move elements to the right)
      !# Negative n: rotate left (move elements to the left)
      class(StringBuffer), intent(inout) :: self
      integer, intent(in) :: rotations
      integer :: n

      if (self%size < 2) then  !# if we have 0 or 1 elem: nothing to do
         return
      end if

      n = mod(rotations, self%size)
      if (n == 0) then  !# 0 rotaions to perform: nothing to do
         return
      end if

      !# convert negative rotation to equivalent positive rotation
      if (n < 0) then
         n = self%size + n
      end if

      !# n is positive (n > 0) if we get here
      self%data = [self%data(self%size + 1 - n:self%size), &
                   self%data(1:self%size - n)]
      call self%adjust_capacity()  ! Must be called! self%data shrank in the previous step
   end subroutine

   function slice(self, start, end, step) result(res)
      !# Slicing, similar to Python's list slicing.
      !# Start and end positions must be positive. Step can be negative.
      !# If `end` is too big, then it'll be truncated to the number of elements.
      !# It returns a new stringbuffer object.
      class(StringBuffer), intent(in) :: self
      integer, intent(in), optional :: start, end, step
      type(StringBuffer) :: res
      integer :: start_val, end_val, step_val
      integer :: i

      step_val = 1          !# default value
      if (present(step)) step_val = step
      call assert(step_val /= 0, "ValueError: slice step cannot be zero")

      if (step_val > 0) then
         start_val = 1           !# default value
         end_val = self%size     !# default value
      else
         !# negative step
         start_val = self%size   !# default value
         end_val = 1             !# default value
      end if

      if (present(start)) start_val = start
      call assert(start_val >= 1, "ValueError: slice start position must be positive")
      if (present(end)) end_val = end
      call assert(end_val >= 1, "ValueError: slice end position must be positive")

      if (step_val > 0) then
         !# positive step
         i = start_val
         do while (i <= min(self%size, end_val))
            call res%append(self%get(i))
            i = i + step_val
         end do
      else
         !# negative step
         i = min(start_val, self%size)
         do while (i >= max(1, end_val))
            call res%append(self%get(i))
            i = i + step_val  !# step is negative, so `i` will be decremented
         end do
      end if
   end function

   pure function find(self, value) result(idx)
      !# Find the index position of an element.
      !# Returns 0 if not found.
      class(StringBuffer), intent(in) :: self
      character(len=*), intent(in) :: value
      integer :: i, idx

      do i = 1, self%size
         if (self%data(i)%s == value) then
            idx = i
            return
         end if
      end do
      idx = 0  !# not found
   end function

   function remove(self, value) result(found)
      !# Remove the first occurrence of `value` from the stringbuffer.
      !# Return true if value was found; return false otherwise.
      class(StringBuffer), intent(inout) :: self
      character(len=*), intent(in) :: value
      logical :: found
      integer :: i

      i = self%find(value)
      if (i == 0) then
         found = .false.
         return
      end if
      !# else, if it was found
      found = .true.
      call self%del(i)
   end function

end module jstringbuffer
