! useful functions in Fortran:
! scan()            - Finds FIRST occurrence of ANY character in a set
! verify(text, set) - Finds FIRST character NOT in a set (optional: back)
!                     return values: 0 - OK, not 0 - first char not in the set

module jstring

! Working with strings and characters.

   use iso_fortran_env, only: stderr => error_unit
   use jassert, only: assert
   use jstringbuffer, only: StringBuffer
   implicit none
   private

   !# see https://jabbalaci.github.io/teaching-assets/hun/python/ascii/asciichart.png
   character, parameter :: NUL = achar(0)    !# '\0'
   character, parameter :: BS = achar(8)     !# backspace
   character, parameter :: TAB = achar(9)    !# '\t'
   character, parameter :: LF = achar(10)    !# '\n'
   character, parameter :: CR = achar(13)    !# '\r'
   character, parameter :: ESC = achar(27)   !#
   character(len=*), parameter :: WHITESPACE = " "//TAB//LF//CR
   character(len=*), parameter :: DIGITS = "0123456789"
   character(len=*), parameter :: ASCII_LETTERS = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
   character(len=*), parameter :: ASCII_LOWERCASE = "abcdefghijklmnopqrstuvwxyz"
   character(len=*), parameter :: ASCII_UPPERCASE = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

   public :: NUL, BS, TAB, LF, CR, ESC, &
             WHITESPACE, DIGITS, &
             ASCII_LETTERS, ASCII_LOWERCASE, ASCII_UPPERCASE

   public :: &          !# Python equivalents:
      capitalize, &     !# "anna".capitalize() -> "Anna"
      center, &         !# "*".center(3) -> " * "
      chomp, &          !# s = s.rstrip("\r\n")
      count_elems, &    !# "Anna".count("n")
      endswith, &       !# "01.png".endswith(".png") -> True
      equal_strings, &  !# same length .and. same content
      explode, &        !# list("abc") -> ["a", "b", "c"]
      find, &           !# "Anna".find("n") -> 1 (Python is 0-based)
      isascii, &        !# "Éva".isascii() -> False
      isdigit, &        !# "2026".isdigit() -> True
      is_in, &          !# "prog" in "programming" -> True
      islower, &        !# "anna".islower() -> True
      isspace, &        !# "   \t    \r\n" -> True
      isupper, &        !# "ANNA".isupper() -> True
      lower, &          !# "aNNa".lower() -> "anna"
      lstrip, &         !# " \t   anna  " -> "anna  "
      removeprefix, &   !# "01.jpg".removeprefix("01") -> ".jpg"
      removesuffix, &   !# "01.jpg".removesuffix(".jpg") -> "01"
      replace, &        !# "cat dog cat".replace("cat", "kitten") -> "kitten dog kitten"
      rev, &            !# "abcd"[::-1] -> "dcba"
      rfind, &          !# "Anna".rfind("n") -> 2 (Python is 0-based)
      rstrip, &         !# "  anna  \n" -> "  anna"
      slice, &          !# like in Python: s[1:5:2], or s[10:2:-2]
      split, &          !# "  aa  bb  cc  ".split() -> ["aa", "bb", "cc"]
      startswith, &     !# "01.png".endswith("01") -> True
      strip, &          !# "  \t    aa    \t   \n".strip() -> "aa"
      swapcase, &       !# "Anna".swapcase() -> "aNNA"
      upper, &          !# "Anna".upper() -> "ANNA"
      zfill             !# "7".zfill(3) -> "007

contains

   function is_in(sub, text) result(result)
      !# Check if a substring is in a text.
      !# Returns true or false.
      !# Like in Python: if "sub" in "subprogram": ...
      character(len=*), intent(in) :: sub
      character(len=*), intent(in) :: text
      logical :: result

      if (len(sub) == 0) then
         result = .true.
         return
      end if

      result = index(text, sub) > 0
   end function

   function find(text, sub) result(result)
      !# Find the position of a substring in a string.
      !# Returns 0 if not found.
      character(len=*), intent(in) :: text
      character(len=*), intent(in) :: sub
      integer :: result

      result = index(text, sub)
   end function

   function rfind(text, sub) result(result)
      !# Find the *last* position of a substring in a string.
      !# Returns 0 if not found.
      character(len=*), intent(in) :: text
      character(len=*), intent(in) :: sub
      integer :: result

      result = index(text, sub, back=.true.)
   end function

   function isdigit(s) result(result)
      !# Are all the characters digits?
      character(len=*), intent(in) :: s
      logical :: result

      if (len(s) == 0) then
         result = .false.
         return
      end if

      result = (verify(s, DIGITS) == 0)
   end function

   function isascii(s) result(result)
      !# Return true if the string is empty or all characters in the string are ASCII,
      !# false otherwise. ASCII characters have code points in the range [0, 127].
      character(len=*), intent(in) :: s
      logical :: result
      integer :: i

      do i = 1, len(s)
         if (iachar(s(i:i)) > 127) then
            result = .false.
            return
         end if
      end do
      result = .true.
   end function

   function isspace(s) result(result)
      !# Is the string/character a whitespace?
      !# Whitespaces: SPACE, TAB, LF (newline), CR (carriage return)
      character(len=*), intent(in) :: s
      logical :: result
      character :: c
      integer :: i

      if (len(s) == 0) then
         result = .false.
         return
      end if

      do i = 1, len(s)
         c = s(i:i)
         if (.not. (c == ' ' .or. c == TAB .or. c == LF .or. c == CR)) then
            result = .false.
            return
         end if
      end do
      result = .true.
   end function

   function islower(s) result(result)
      !# Is the string/character lowercased?
      !# It only works with ASCII text.
      character(len=*), intent(in) :: s
      logical :: result
      integer :: code  !# ASCII code of a character
      integer :: i

      if (len(s) == 0) then
         result = .false.
         return
      end if

      do i = 1, len(s)
         code = iachar(s(i:i))
         if ((code < 97) .or. (code > 122)) then
            result = .false.
            return
         end if
      end do
      result = .true.
   end function

   function isupper(s) result(result)
      !# Is the string/character uppercased?
      !# It only works with ASCII text.
      character(len=*), intent(in) :: s
      logical :: result
      integer :: code  !# ASCII code of a character
      integer :: i

      if (len(s) == 0) then
         result = .false.
         return
      end if

      do i = 1, len(s)
         code = iachar(s(i:i))
         if ((code < 65) .or. (code > 90)) then
            result = .false.
            return
         end if
      end do
      result = .true.
   end function

   function upper(s) result(t)
      !# Returns string `s` in uppercase.
      !# It only works with ASCII text.
      character(len=*), intent(in) :: s
      character(len=len(s)) :: t
      integer :: i, code

      t = s
      do i = 1, len(t)
         code = iachar(t(i:i))
         if ((code >= 97) .and. (code <= 122)) then
            t(i:i) = achar(iachar(t(i:i)) - 32)
         end if
      end do
   end function

   function lower(s) result(t)
      !# Returns string `s` in lowercase.
      !# It only works with ASCII text.
      character(len=*), intent(in) :: s
      character(len=len(s)) :: t
      integer :: i, code

      t = s
      do i = 1, len(t)
         code = iachar(t(i:i))
         if ((code >= 65) .and. (code <= 90)) then
            t(i:i) = achar(iachar(t(i:i)) + 32)
         end if
      end do
   end function

   function strip(s) result(result)
      !# Remove leading and trailing whitespaces.
      character(len=*), intent(in) :: s
      character(len=:), allocatable :: result
      integer :: left, right

      left = verify(s, WHITESPACE)
      right = verify(s, WHITESPACE, back=.true.)

      if (left == 0 .or. right == 0) then
         result = ""
         return
      end if

      result = s(left:right)
   end function

   function lstrip(s) result(result)
      !# Remove leading whitespaces (from the beginning).
      character(len=*), intent(in) :: s
      character(len=:), allocatable :: result
      integer :: left, right

      left = verify(s, WHITESPACE)
      right = len(s)

      if (left == 0 .or. right == 0) then
         result = ""
         return
      end if

      result = s(left:right)
   end function

   function rstrip(s, chars) result(result)
      !# Remove trailing chars (from the end).
      !# If `chars` not provided, then it defaults to removing whitespace characters.
      character(len=*), intent(in) :: s
      character(len=*), intent(in), optional :: chars
      character(len=:), allocatable :: chars_val
      character(len=:), allocatable :: result
      integer :: left, right

      chars_val = WHITESPACE  !# default value
      if (present(chars)) chars_val = chars

      left = 1
      right = verify(s, chars_val, back=.true.)

      if (right == 0) then
         result = ""
         return
      end if

      result = s(left:right)
   end function

   function chomp(s) result(result)
      !# Remove trailing "\r" and "\n" characters (from the end).
      character(len=*), intent(in) :: s
      character(len=:), allocatable :: result

      result = rstrip(s, CR//LF)
   end function

   function startswith(text, sub) result(result)
      !# Check if the string starts with a given substring.
      character(len=*), intent(in) :: text
      character(len=*), intent(in) :: sub
      logical :: result

      result = (index(text, sub) == 1)
   end function

   function endswith(text, sub) result(result)
      !# Check if the string ends with a given substring.
      character(len=*), intent(in) :: text
      character(len=*), intent(in) :: sub
      logical :: result
      integer :: pos

      pos = index(text, sub, back=.true.)
      if (pos == 0) then  !# not found
         result = .false.
         return
      end if
      !# else
      result = (pos == len(text) - len(sub) + 1)
   end function

   function replace(s, old, new, count) result(t)
      !# In `s`, replace every occurrence of `old` with `new`.
      !# `count` (optional): number of replaces
      !# Like Python's str.replace()
      !# Not efficient for very long strings (it has lots of slicings and concatenations).
      character(len=*), intent(in) :: s, old, new
      character(len=:), allocatable :: t
      integer, intent(in), optional :: count
      integer :: i, old_len, s_len, limit, counter
      logical :: do_changes

      limit = -1
      if (present(count)) limit = count

      if (limit == 0) then
         t = s
         return
      end if

      t = ""
      s_len = len(s)
      old_len = len(old)

      do_changes = .true.
      counter = 0
      i = 1
      do while (i <= s_len)
         if (do_changes .and. s(i:min(s_len, i + old_len - 1)) == old) then
            t = t//new
            i = i + old_len
            if (limit > 0) then  !# taking `count` into account
               counter = counter + 1  !# number of replaces
               if (counter >= limit) then
                  do_changes = .false.
               end if
            end if
         else
            t = t//s(i:i)
            i = i + 1
         end if
      end do
   end function

   function count_elems(s, sub) result(result)
      !# In `s`, the substring `sub` is present how many times?
      !# No overlapping. Ex.: in "aaa", "aa" is present just once.
      character(len=*), intent(in) :: s, sub
      integer :: i, result, sub_len

      if (sub == "") then
         result = len(s) + 1
         return
      end if

      result = 0
      sub_len = len(sub)

      i = 1
      do while (i <= len(s))
         if (s(i:min(len(s), i + sub_len - 1)) == sub) then
            result = result + 1
            i = i + sub_len
         else
            i = i + 1
         end if
      end do
   end function

   function slice(s, start, end, step) result(t)
      !# String slicing similar to Python.
      !# Start and end positions must be positive. Step can be negative.
      !# If `end` is too big, then it'll be truncated to the string's length.
      !# Not efficient for very long strings (it has lots of concatenations).
      character(len=*), intent(in) :: s
      integer, intent(in), optional :: start, end, step
      character(len=:), allocatable :: t
      integer :: start_val, end_val, step_val
      integer :: i, s_len

      t = ""
      s_len = len(s)

      step_val = 1          !# default value
      if (present(step)) step_val = step
      call assert(step_val /= 0, "ValueError: slice step cannot be zero")

      if (step_val > 0) then
         start_val = 1      !# default value
         end_val = len(s)   !# default value
      else
         !# negative step
         start_val = len(s) !# default value
         end_val = 1        !# default value
      end if

      if (present(start)) start_val = start
      call assert(start_val >= 1, "ValueError: slice start position must be positive")
      if (present(end)) end_val = end
      call assert(end_val >= 1, "ValueError: slice end position must be positive")

      if (step_val > 0) then
         !# positive step
         i = start_val
         do while (i <= min(s_len, end_val))
            t = t//s(i:i)
            i = i + step_val
         end do
      else
         !# negative step
         i = min(start_val, s_len)
         do while (i >= max(1, end_val))
            t = t//s(i:i)
            i = i + step_val  !# step is negative, so `i` will be decremented
         end do
      end if
   end function

   function rev(s) result(t)
      !# Returns the reversed string, e.g. "abc" -> "cba"
      !# Works with ASCII only.
      character(len=*), intent(in) :: s
      character(len=len(s)) :: t
      character tmp
      integer :: i, j

      t = s
      i = 1; j = len(t)
      do while (i < j)
         tmp = t(i:i)
         t(i:i) = t(j:j)
         t(j:j) = tmp
         i = i + 1
         j = j - 1
      end do
   end function

   ! function number_of_tokens_with_whitespaces(s) result(counter)  !# private, helper function
   !    character(len=*), intent(in) :: s
   !    integer :: counter
   !    integer, parameter :: OUTSIDE = 0
   !    integer, parameter :: INSIDE = 1
   !    integer :: i, state, start

   !    counter = 0
   !    state = OUTSIDE
   !    do i = 1, len(s)
   !       if (.not. isspace(s(i:i))) then
   !          if (state == OUTSIDE) then
   !             state = INSIDE
   !             start = i
   !          end if
   !       else
   !          !# if isspace
   !          if (state == INSIDE) then
   !             state = OUTSIDE
   !             counter = counter + 1
   !             ! write (stderr, '(*(g0))') "'", s(start:i - 1), "'"
   !          end if
   !       end if
   !    end do
   !    if (state == INSIDE) then
   !       counter = counter + 1
   !       ! write (stderr, '(*(g0))') "'", s(start:len(s)), "'"
   !    end if
   ! end function

   ! function number_of_tokens(s, delimiter) result(counter)
   !    character(len=*), intent(in) :: s
   !    character(len=*), intent(in), optional :: delimiter
   !    integer :: i, counter, len_s, len_delimiter, state, start
   !    integer, parameter :: OUTSIDE = 0
   !    integer, parameter :: INSIDE = 1

   !    if (.not. present(delimiter)) then
   !       !# consider whitespaces between the tokens
   !       counter = number_of_tokens_with_whitespaces(s)
   !       return
   !    end if
   !    !# else, a delimiter is present
   !    len_delimiter = len(delimiter)
   !    call assert(len_delimiter > 0, "ValueError: empty separator")

   !    counter = 0
   !    len_s = len(s)
   !    state = INSIDE
   !    start = 1
   !    i = 1
   !    do while (i <= len_s)
   !       if (s(i:min(len_s, i + len_delimiter - 1)) == delimiter) then
   !          state = OUTSIDE
   !          counter = counter + 1
   !          ! write (stderr, '(*(g0))') "'", s(start:i - 1), "'"
   !          i = i + len_delimiter
   !          start = i
   !       else
   !          !# `i` doesn't stand at the beginning of a delimiter
   !          if (state == OUTSIDE) then
   !             state = INSIDE
   !             start = i
   !          end if
   !          i = i + 1
   !       end if
   !    end do
   !    if (endswith(s, delimiter)) then
   !       state = INSIDE
   !    end if
   !    if (state == INSIDE) then
   !       counter = counter + 1
   !       ! write (stderr, '(*(g0))') "'", s(start:len(s)), "'"
   !    end if
   ! end function

   function split_with_whitespaces(s) result(sb)  !# private, helper function
      !# When no delimiter is provided.
      !# Like in Python: result = s.split()
      character(len=*), intent(in) :: s
      type(StringBuffer) :: sb
      integer :: counter
      integer, parameter :: OUTSIDE = 0
      integer, parameter :: INSIDE = 1
      integer :: i, state, start

      counter = 0
      state = OUTSIDE
      do i = 1, len(s)
         if (.not. isspace(s(i:i))) then
            if (state == OUTSIDE) then
               state = INSIDE
               start = i
            end if
         else
            !# if isspace
            if (state == INSIDE) then
               state = OUTSIDE
               counter = counter + 1
               ! write (stderr, '(*(g0))') "'", s(start:i - 1), "'"
               call sb%append(s(start:i - 1))
            end if
         end if
      end do
      if (state == INSIDE) then
         counter = counter + 1
         ! write (stderr, '(*(g0))') "'", s(start:len(s)), "'"
         call sb%append(s(start:len(s)))
      end if
   end function

   function split(s, delimiter) result(sb)
      !# Similar to Python's str.split() method.
      !# It returns a StringBuffer, which is like a list of strings.
      character(len=*), intent(in) :: s
      character(len=*), intent(in), optional :: delimiter
      type(StringBuffer) :: sb
      integer :: i, counter, len_s, len_delimiter, state, start
      integer, parameter :: OUTSIDE = 0
      integer, parameter :: INSIDE = 1

      if (.not. present(delimiter)) then
         !# consider whitespaces between the tokens
         sb = split_with_whitespaces(s)
         return
      end if
      !# else, a delimiter is present
      len_delimiter = len(delimiter)
      call assert(len_delimiter > 0, "ValueError: empty separator")

      counter = 0
      len_s = len(s)
      state = INSIDE
      start = 1
      i = 1
      do while (i <= len_s)
         if (s(i:min(len_s, i + len_delimiter - 1)) == delimiter) then
            state = OUTSIDE
            counter = counter + 1
            ! write (stderr, '(*(g0))') "'", s(start:i - 1), "'"
            call sb%append(s(start:i - 1))
            i = i + len_delimiter
            start = i
         else
            !# `i` doesn't stand at the beginning of a delimiter
            if (state == OUTSIDE) then
               state = INSIDE
               start = i
            end if
            i = i + 1
         end if
      end do
      if (endswith(s, delimiter)) then
         state = INSIDE
      end if
      if (state == INSIDE) then
         counter = counter + 1
         ! write (stderr, '(*(g0))') "'", s(start:len(s)), "'"
         call sb%append(s(start:len(s)))
      end if
   end function

   function removeprefix(s, prefix) result(result)
      !# Removes the given prefix from the beginning of `s`.
      !# Returns `s` without the prefix.
      character(len=*), intent(in) :: s
      character(len=*), intent(in) :: prefix
      character(len=:), allocatable :: result

      if (.not. startswith(s, prefix)) then
         result = s
         return
      end if
      !# else:
      result = s(len(prefix) + 1:)
   end function

   function removesuffix(s, suffix) result(result)
      !# Removes the given suffix from the end of `s`.
      !# Returns `s` without the suffix.
      character(len=*), intent(in) :: s
      character(len=*), intent(in) :: suffix
      character(len=:), allocatable :: result

      if (.not. endswith(s, suffix)) then
         result = s
         return
      end if
      !# else:
      result = s(1:len(s) - len(suffix))
   end function

   function zfill(s, length) result(result)
      character(len=*), intent(in) :: s
      integer, intent(in) :: length
      character(len=:), allocatable :: result
      integer :: diff

      if (length <= len(s)) then
         result = s
         return
      end if
      !# else:
      diff = length - len(s)
      result = repeat("0", diff)//s
   end function

   function capitalize(s) result(result)
      !# Capitalizes `s`: makes the first letter uppercase, and the rest lowercase.
      character(len=*), intent(in) :: s
      character(len=:), allocatable :: result

      if (len(s) == 0) then
         result = s
         return
      end if
      !# else
      result = upper(s(1:1))//lower(s(2:))
   end function

   function swapcase(s) result(t)
      !# Swaps the case of every character in `s`.
      !# Lowercase becomes uppercase, and vice versa.
      character(len=*), intent(in) :: s
      character(len=:), allocatable :: t
      integer :: i

      t = s
      do i = 1, len(t)
         if (isupper(t(i:i))) then
            t(i:i) = lower(t(i:i))
         else if (islower(t(i:i))) then
            t(i:i) = upper(t(i:i))
         end if
      end do
   end function

   function equal_strings(s1, s2) result(result)
      !# Checks if two strings are equal.
      !# "*" == "* " is true in Fortran since the first string will be padded to length 2.
      !# This function also verifies if the strings have the same length.
      character(len=*), intent(in) :: s1, s2
      logical :: result

      result = (len(s1) == len(s2)) .and. (s1 == s2)
   end function

   function center(s, width) result(result)
      !# Centers a string of a given width.
      character(len=*), intent(in) :: s
      integer, intent(in) :: width
      character(len=:), allocatable :: result
      integer :: diff, half

      if (width <= len(s)) then
         result = s
         return
      end if
      !# else:
      diff = width - len(s)
      half = diff / 2
      result = repeat(" ", half)//s//repeat(" ", half)
      if (mod(diff, 2) == 1) then
         result = result//" "
      end if
   end function

   function explode(s) result(result)
      !# Converts a string to a list of characters.
      character(len=*), intent(in) :: s
      type(StringBuffer) :: result
      integer :: i

      do i = 1, len(s)
         call result%append(s(i:i))
      end do
   end function

end module jstring
