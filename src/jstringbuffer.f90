module jstringbuffer

! A stringbuffer for storing variable-length strings.

   use jassert, only: assert
   implicit none
   private
   integer, parameter :: INITIAL_CAPACITY = 8

   public :: String, StringBuffer

   type :: String
      !# wrapper type (box) for variable-length strings
      character(len=:), allocatable :: s
   end type String

   type :: StringBuffer
      integer, private :: size = 0  ! number of elems
      integer, private :: capacity = INITIAL_CAPACITY  ! initial capacity
      type(String), private, allocatable :: data(:)
   contains
      procedure, private :: quicksort
      procedure, public :: append, debug, stats, number_of_elems, total_length, &
         to_string, join, &
         equals, clear, get, sort, sorted, copy, set, is_sorted, pop, is_empty, &
         contains, add_to_set, get_capacity, set_capacity, last, min_elem, max_elem, &
         count_elems, swap
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

   function total_length(self) result(result)
      !# Sum of the length of every element in the stringbuffer.
      !# If we call to_string(), we get a string of this length.
      class(StringBuffer), intent(in) :: self
      integer :: i, result

      result = 0
      do i = 1, self%size
         result = result + len(self%data(i)%s)
      end do
   end function

   function number_of_elems(self) result(result)
      !# Number of elements (boxes) in the stringbuffer.
      !# This is NOT equivalent to the total length!
      class(StringBuffer), intent(in) :: self
      integer :: result
      result = self%size
   end function

   function get_capacity(self) result(result)
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

   function is_empty(self) result(result)
      class(StringBuffer), intent(in) :: self
      logical :: result

      result = (self%size == 0)
   end function

   subroutine append(self, value)
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

      do i = 1, self%size
         print '(*(g0))', i, ": ", "'", self%data(i)%s, "'"
      end do
   end subroutine

   subroutine stats(self)
      !# Print some statistics about the stringbuffer. For debug purposes.
      class(StringBuffer), intent(in) :: self

      print '(a)', "stats:"
      print '(*(g0))', " number of elems: ", self%size
      print '(*(g0))', " capacity: ", self%capacity
      print '(*(g0))', " total length: ", self%total_length()
   end subroutine

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

   function to_string(self) result(result)
      !# Get the content of the stringbuffer as a string.
      class(StringBuffer), intent(in) :: self
      character(len=:), allocatable :: result
      integer :: i, j, length

      if (.not. allocated(self%data)) then
         result = ""
      else
         allocate (character(len=self%total_length()) :: result)
         j = 1
         do i = 1, self%size
            associate (s => self%data(i)%s)
               length = len(s)
               result(j:j + length - 1) = s
               j = j + length
            end associate
         end do
      end if
   end function

   function join(self, sep) result(result)
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

   function equals(self, other) result(result)
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

   function copy(self) result(result)
      !# Creates and returns an independent copy of itself.
      class(StringBuffer), intent(in) :: self
      type(StringBuffer) :: result

      result%size = self%size
      result%capacity = self%capacity
      result%data = self%data
   end function

   function is_sorted(self) result(result)
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

   function pop(self) result(result)
      !# Take out and return the last element of the stringbuffer.
      !# It modifies the stringbuffer!
      !# TODO: make it possible to take out any element (using an index)
      class(StringBuffer), intent(inout) :: self
      character(len=:), allocatable :: result

      call assert(self%size > 0, "Error: the stringbuffer is empty")

      result = self%data(self%size)%s
      self%data = self%data(1:self%size - 1)
      self%size = self%size - 1
      if (self%size == 0) then
         call self%clear()
      end if
   end function

   function contains(self, value) result(result)
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

   function count_elems(self, value) result(result)
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

end module jstringbuffer
