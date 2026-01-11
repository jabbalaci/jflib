module jintlist

! A dynamic array for storing integers.
! Currently, it only has some very basic operations.

   implicit none
   private
   integer, parameter :: INITIAL_CAPACITY = 8

   public :: IntList

   type :: IntList
      integer, private :: size = 0  ! number of elems
      integer, private :: capacity = INITIAL_CAPACITY  ! initial capacity
      integer, private, allocatable :: data(:)
   contains
      procedure :: &
         append, &               !# append a new element to the end
         debug, &                !# show the content (for debug purposes)
         number_of_elems, &      !# number of stored elements (aka length, size)
         to_array                !# get the stored elements as an array of integers
   end type IntList

contains

   function number_of_elems(self) result(result)
      !# Number of elements in the list.
      class(IntList), intent(in) :: self
      integer :: result

      result = self%size
   end function

   function to_array(self) result(result)
      !# Get the stored elements as an array of integers.
      class(IntList), intent(in) :: self
      integer, allocatable :: result(:)

      if (.not. allocated(self%data)) then
         allocate (result(0))
      else
         result = self%data(1:self%size)
      end if
   end function

   subroutine append(self, value)
      !# Append an element to the end of the list.
      class(IntList), intent(inout) :: self
      integer, intent(in) :: value
      integer, allocatable :: temp(:)

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
      self%data(self%size) = value
   end subroutine

   subroutine debug(self)
      !# Print the content to stdout. For debug purposes.
      class(IntList), intent(in) :: self
      integer :: i

      print '(a)', "debug:"
      if (self%size == 0) then
         print *, "(empty)"
      else
         do i = 1, self%size
            print '(x,*(g0))', i, ": ", self%data(i)
         end do
      end if
   end subroutine

end module
