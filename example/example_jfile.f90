program example_jfile
   use jfile
   use jstringbuffer, only: StringBuffer
   implicit none
   integer :: iostat
   character(len=:), allocatable :: fname
   type(StringBuffer) :: lines
   character(len=:), allocatable :: content

   fname = "test/files/read.txt"
   lines = readlines(fname, iostat)
   print '(*(g0))', "iostat: ", iostat
   call lines%debug()
   print '(a)', "---"
   fname = "not_exists.txt"
   lines = readlines(fname, iostat)
   print '(*(g0))', "iostat: ", iostat
   print '(a)', "---"
   fname = "test/files/read.txt"
   content = read_content(fname, iostat)
   print '(*(g0))', '"', content, '"'
end program
