program test_jfile
   use jassert, only: assert
   use jfile
   use jstringbuffer, only: StringBuffer
   implicit none

   call test_read_content()
   call test_readlines()

   print '(a)', "OK"
contains

   subroutine test_read_content()
      integer :: iostat
      character(len=:), allocatable :: fname
      character(len=:), allocatable :: content

      fname = "not_exists.txt"
      content = read_content(fname, iostat)
      call assert(iostat /= 0)
      !#
      fname = "test/files/hello.txt"
      content = read_content(fname, iostat)
      call assert(iostat == 0)
      call assert(content == "hello")
   end subroutine

   subroutine test_readlines()
      integer :: iostat
      character(len=:), allocatable :: fname
      type(StringBuffer) :: lines

      fname = "not_exists.txt"
      lines = readlines(fname, iostat)
      call assert(iostat /= 0)
      !#
      fname = "test/files/hello.txt"
      lines = readlines(fname, iostat)
      call assert(iostat == 0)
      call assert(lines%number_of_elems() == 1)
      call assert(lines%get(1) == "hello")
      !#
      fname = "test/files/read.txt"
      lines = readlines(fname, iostat)
      call assert(iostat == 0)
      call assert(lines%number_of_elems() == 3)
      call assert(lines%get(1) == "first line")
      call assert(lines%get(2) == "second line")
      call assert(lines%get(3) == "third line")
   end subroutine

end program
