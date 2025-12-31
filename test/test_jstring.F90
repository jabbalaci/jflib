program test_jstring
   use jassert
   use jstring
   use jconstants
   use jstringbuffer, only: StringBuffer
   implicit none

   call test_lower()
   call test_upper()
   call test_islower()
   call test_isupper()
   call test_isspace()
   call test_find()
   call test_rfind()
   call test_is_in()
   call test_isdigit()
   call test_isascii()
   call test_strip()
   call test_lstrip()
   call test_rstrip()
   call test_startswith()
   call test_endswith()
   call test_replace()
   call test_replace_with_count()
   call test_rev()
   call test_slice()
   call test_slice_negative()
   ! call test_number_of_tokens_with_witespace()
   ! call test_number_of_tokens_with_delimiter()
   call test_split_with_witespace()
   call test_split_with_delimiter()

   print '(a)', "OK"

contains

   subroutine test_lower()
      call assert(lower("") == "")
      call assert(lower("a") == "a")
      call assert(lower("ab") == "ab")
      call assert(lower("ab,cd") == "ab,cd")
      call assert(lower("A") == "a")
      call assert(lower("AB") == "ab")
      call assert(lower("AB,CD") == "ab,cd")
      call assert(lower("abc   ") == "abc   ")
      call assert(lower("   ABC   ") == "   abc   ")
      call assert(lower("G();") == "g();")
   end subroutine

   subroutine test_upper()
      call assert(upper("") == "")
      call assert(upper("a") == "A")
      call assert(upper("ab") == "AB")
      call assert(upper("ab,cd") == "AB,CD")
      call assert(upper("A") == "A")
      call assert(upper("AB") == "AB")
      call assert(upper("AB,CD") == "AB,CD")
      call assert(upper("abc   ") == "ABC   ")
      call assert(upper("   ABC   ") == "   ABC   ")
      call assert(upper("g();") == "G();")
   end subroutine

   subroutine test_islower()
      call assert_false(islower(""))
      call assert_false(islower("A"))
      call assert_false(islower("AB"))
      call assert_false(islower("ABC"))
      !#
      call assert_true(islower("a"))
      call assert_true(islower("ab"))
      call assert_true(islower("abc"))
      !#
      call assert_false(islower("Anna"))
      call assert_false(islower("abc;def"))
      call assert_true(islower("abcdef"))
   end subroutine

   subroutine test_isupper()
      call assert_false(isupper(""))
      call assert_true(isupper("A"))
      call assert_true(isupper("AB"))
      call assert_true(isupper("ABC"))
      !#
      call assert_false(isupper("a"))
      call assert_false(isupper("ab"))
      call assert_false(isupper("abc"))
      !#
      call assert_false(isupper("Anna"))
      call assert_false(isupper("ABC;DEF"))
      call assert_true(isupper("ABCDEF"))
   end subroutine

   subroutine test_isspace()
      call assert_false(isspace(""))
      call assert_true(isspace(" "))
      call assert_true(isspace("  "))
      call assert_true(isspace("  "//TAB))
      call assert_true(isspace("  "//TAB//"  "))
      call assert_false(isspace(" x "//TAB//"  "))
      call assert_false(isspace("  "//TAB//" x "))
      call assert_true(isspace("  "//TAB//"  "//LF))
      call assert_true(isspace("  "//TAB//"  "//LF//" "))
      call assert_true(isspace("   "//CR//LF))
      call assert_false(isspace("line"//CR//LF))
   end subroutine

   subroutine test_find()
      call assert(find("", "k") == 0)
      call assert(find("joker", "k") == 3)
      call assert(find("joker", "ke") == 3)
      call assert(find("joker", "j") == 1)
      call assert(find("joker", "xxx") == 0)
   end subroutine

   subroutine test_rfind()
      call assert(find("aladar", "a") == 1)  !# find
      !#
      call assert(rfind("", "k") == 0)
      call assert(rfind("aladar", "a") == 5)
      call assert(rfind("aladar", "xxx") == 0)
      call assert(rfind("aa,bb,aa,cc", "aa") == 7)
   end subroutine

   subroutine test_is_in()
      call assert_true(is_in("sub", "subprogram"))
      call assert_false(is_in("xxx", "subprogram"))
      call assert_true(is_in("j", "joker"))
      call assert_true(is_in("", "joker"))
      call assert_true(is_in("jo", "joker"))
      call assert_true(is_in("k", "joker"))
      call assert_true(is_in("ke", "joker"))
      call assert_true(is_in("r", "joker"))
      call assert_false(is_in("t", "joker"))
   end subroutine

   subroutine test_isdigit()
      call assert_false(isdigit(""))
      call assert_false(isdigit("-1"))
      call assert_true(isdigit("0"))
      call assert_true(isdigit("2"))
      call assert_true(isdigit("2026"))
   end subroutine

   subroutine test_isascii()
      call assert_true(isascii(""))
      call assert_true(isascii("Laci"))
      call assert_true(isascii("Laci 2026"))
      !#
      call assert_false(isascii("é"))
      call assert_false(isascii("Éva"))
      call assert_false(isascii("László"))
   end subroutine

   subroutine test_strip()
      call assert(strip("abcd") == "abcd")
      call assert(strip("  abcd") == "abcd")
      call assert(strip("abcd  ") == "abcd")
      call assert(strip("  abcd  ") == "abcd")
      call assert(strip("  ab  cd  ") == "ab  cd")
      call assert(strip("") == "")
      call assert(strip("  a  ") == "a")
      call assert(strip("line"//LF) == "line")
      call assert(strip("line"//CR//LF) == "line")
      call assert(strip("  "//TAB//"  ") == "")
      call assert(strip(WHITESPACE) == "")
      call assert(strip(WHITESPACE//"a"//WHITESPACE) == "a")
   end subroutine

   subroutine test_lstrip()
      call assert(lstrip("abcd") == "abcd")
      call assert(lstrip("  abcd") == "abcd")
      call assert(lstrip("abcd  ") == "abcd  ")
      call assert(lstrip("  abcd  ") == "abcd  ")
      call assert(lstrip("  ab  cd  ") == "ab  cd  ")
      call assert(lstrip("") == "")
      call assert(lstrip("  a  ") == "a  ")
      call assert(lstrip("line"//LF) == "line"//LF)
      call assert(lstrip("line"//CR//LF) == "line"//CR//LF)
      call assert(lstrip("  "//TAB//"  ") == "")
      call assert(lstrip(WHITESPACE) == "")
      call assert(lstrip(WHITESPACE//"a"//WHITESPACE) == "a"//WHITESPACE)
   end subroutine

   subroutine test_rstrip()
      call assert(rstrip("abcd") == "abcd")
      call assert(rstrip("  abcd") == "  abcd")
      call assert(rstrip("abcd  ") == "abcd")
      call assert(rstrip("  abcd  ") == "  abcd")
      call assert(rstrip("  ab  cd  ") == "  ab  cd")
      call assert(rstrip("") == "")
      call assert(rstrip("  a  ") == "  a")
      call assert(rstrip("line"//LF) == "line")
      call assert(rstrip("line"//CR//LF) == "line")
      call assert(rstrip("  "//TAB//"  ") == "")
      call assert(rstrip(WHITESPACE) == "")
      call assert(rstrip(WHITESPACE//"a"//WHITESPACE) == WHITESPACE//"a")
   end subroutine

   subroutine test_startswith()
      character(len=*), parameter :: url = "https://example.com/gallery/01.png"
      call assert_true(startswith(url, "http"))
      call assert_false(startswith(url, "ftp"))
      call assert_true(startswith(url, "https"))
      call assert_true(startswith(url, "https://"))
      call assert_true(startswith("abc", "abc"))
      call assert_true(startswith("ab", "ab"))
      call assert_true(startswith("a", "a"))
      call assert_true(startswith("", ""))
      call assert_false(startswith("", "a"))
      call assert_true(startswith("a", ""))
   end subroutine

   subroutine test_endswith()
      character(len=*), parameter :: url = "https://example.com/gallery/01.png"
      call assert_true(endswith(url, ""))
      call assert_true(endswith(url, "g"))
      call assert_true(endswith(url, "png"))
      call assert_true(endswith(url, ".png"))
      call assert_false(endswith(url, ".jpg"))
      call assert_true(endswith("abc", "abc"))
      call assert_true(endswith("ab", "ab"))
      call assert_true(endswith("a", "a"))
      call assert_true(endswith("", ""))
      call assert_false(endswith("", "a"))
      call assert_true(endswith("a", ""))
   end subroutine

   subroutine test_replace()
      call assert(replace("banana", "na", "ABA") == "baABAABA")
      call assert(replace("banana", "ba", "ABA") == "ABAnana")
      call assert(replace("", "ba", "ABA") == "")
      call assert(replace("aaa", "a", "aa") == "aaaaaa")
      call assert(replace("abcd", "d", "") == "abc")
      call assert(replace("http://example.com", "http", "https") == "https://example.com")
      call assert(replace("01.jpg", ".jpg", ".png") == "01.png")
      call assert(replace("cat dog cat cat", "cat", "kitten") == "kitten dog kitten kitten")
   end subroutine

   subroutine test_replace_with_count()
      character(len=*), parameter :: text = "cat dog cat cat"
      call assert(replace(text, "cat", "kitten") == "kitten dog kitten kitten")
      call assert(replace(text, "cat", "kitten", -1) == "kitten dog kitten kitten")
      call assert(replace(text, "cat", "kitten", -2) == "kitten dog kitten kitten")
      call assert(replace(text, "cat", "kitten", -100) == "kitten dog kitten kitten")
      call assert(replace(text, "cat", "kitten", 0) == "cat dog cat cat")
      call assert(replace(text, "cat", "kitten", 1) == "kitten dog cat cat")
      call assert(replace(text, "cat", "kitten", 2) == "kitten dog kitten cat")
      call assert(replace(text, "cat", "kitten", 3) == "kitten dog kitten kitten")
      call assert(replace(text, "cat", "kitten", 4) == "kitten dog kitten kitten")
      call assert(replace(text, "cat", "kitten", 100) == "kitten dog kitten kitten")
   end subroutine

   subroutine test_slice()
      call assert("abc"(1:1) == "a")
      call assert("abc"(2:2) == "b")
      call assert("abc"(3:3) == "c")
      !#
      call assert(slice("abc") == "abc")
      call assert(slice("123456789", 1, 3) == "123")
      call assert(slice("123456789", 1, 100) == "123456789")
      call assert(slice("123456789", 3, 4) == "34")
      call assert(slice("123456789", 3, 3) == "3")
      call assert(slice("123456789", 9, 9) == "9")
      call assert(slice("123456789", 3, 2) == "")
      call assert(slice("123456789", 20, 30) == "")
      call assert(slice("123456789", 20, 5) == "")
      call assert(slice("123456789", 1, 9, 2) == "13579")
      call assert(slice("123456789", 1, 5, 2) == "135")
      call assert(slice("123456789", 1, 6, 2) == "135")
      call assert(slice("123456789", 1, 9, 1) == "123456789")
      call assert(slice("123456789", 4, 2, 1) == "")
      call assert(slice("123456789", 4, 4, 1) == "4")
      ! call assert(slice("123456789", 1, 9, 0) == "123456789")  !# fail
      call assert(slice("Laci", 2) == "aci")
      call assert(slice("Laci", 1, 3) == "Lac")
   end subroutine

   subroutine test_slice_negative()
      ! call assert("abc"(-1:-1) == "c")  !# error, there's no negative indexing in Fortran
      call assert(slice("123456789", 4, 3, -1) == "43")
      call assert(slice("123456789", 9, 1, -1) == "987654321")
      call assert(slice("123456789", 9, 1, -2) == "97531")
      call assert(slice("123456789", 8, 1, -2) == "8642")
      call assert(slice("123456789", 5, 1, -1) == "54321")
      call assert(slice("123456789", 20, 1, -1) == "987654321")
      call assert(slice("123456789", step=-1) == "987654321")
      call assert(slice("123456789", step=-1) == rev("123456789"))
   end subroutine

   subroutine test_rev()
      call assert(rev("") == "")
      call assert(rev("a") == "a")
      call assert(rev("ab") == "ba")
      call assert(rev("abc") == "cba")
      call assert(rev("xxx") == "xxx")
      call assert(rev("Laci") == "icaL")
   end subroutine

   ! subroutine test_number_of_tokens_with_witespace()
   !    call assert(number_of_tokens("") == 0)
   !    call assert(number_of_tokens("aa") == 1)
   !    call assert(number_of_tokens("aa bb") == 2)
   !    call assert(number_of_tokens("aa   bb") == 2)
   !    call assert(number_of_tokens("  aa  ") == 1)
   !    call assert(number_of_tokens("  aa  bb    cc     ") == 3)
   !    call assert(number_of_tokens("       ") == 0)
   !    call assert(number_of_tokens(WHITESPACE) == 0)
   !    call assert(number_of_tokens("  aa  bb   "//TAB//"   cc     ") == 3)
   ! end subroutine

   ! subroutine test_number_of_tokens_with_delimiter()
   !    ! call assert(number_of_tokens("aa bb", "") == 0)  !# error, empty delimiter
   !    call assert(number_of_tokens("aa,bb", ",") == 2)
   !    call assert(number_of_tokens("aa", ",") == 1)
   !    call assert(number_of_tokens("aa,bb,cc", ",") == 3)
   !    call assert(number_of_tokens(" aa,bb,cc ", ",") == 3)
   !    call assert(number_of_tokens("aa bb cc", " ") == 3)
   !    call assert(number_of_tokens("aa,bb", ",") == 2)
   !    call assert(number_of_tokens(",aa,bb", ",") == 3)
   !    call assert(number_of_tokens(",aa,bb,", ",") == 4)
   !    call assert(number_of_tokens("aa bb", " ") == 2)
   !    call assert(number_of_tokens(" aa bb", " ") == 3)
   !    call assert(number_of_tokens(" aa bb ", " ") == 4)
   !    call assert(number_of_tokens(",aa,bb,", ",") == 4)
   !    call assert(number_of_tokens("", ",") == 1)
   !    call assert(number_of_tokens(",", ",") == 2)
   !    call assert(number_of_tokens(",,", ",") == 3)
   !    call assert(number_of_tokens("  a  b  c", " ") == 7)
   !    call assert(number_of_tokens("a  b  c", " ") == 5)
   !    call assert(number_of_tokens("a  b  c  ", " ") == 7)
   !    call assert(number_of_tokens("user1;100;laci", ";") == 3)
   !    call assert(number_of_tokens("user1|||100|||laci", "|||") == 3)
   !    call assert(number_of_tokens("a,a", ",") == 2)
   !    call assert(number_of_tokens("  a  b  c  ", " ") == 9)
   !    call assert(number_of_tokens("a b", " ") == 2)
   !    call assert(number_of_tokens("a  b", " ") == 3)
   !    call assert(number_of_tokens("a,,b", ",") == 3)
   !    call assert(number_of_tokens("a---b---c", "---") == 3)
   !    call assert(number_of_tokens("a---b--c", "---") == 2)
   ! end subroutine

   subroutine test_split_with_witespace()
      type(StringBuffer) :: got
      type(StringBuffer) :: expected

      got = split("")
      call assert_true(got%equals(expected))
      call got%clear(); call expected%clear()
      !#
      got = split("aa")
      call expected%append("aa")
      call assert_true(got%equals(expected))
      call got%clear(); call expected%clear()
      !#
      got = split("aa bb")
      call expected%append("aa"); call expected%append("bb")
      call assert_true(got%equals(expected))
      call got%clear(); call expected%clear()
      !#
      got = split("aa   bb")
      call expected%append("aa"); call expected%append("bb")
      call assert_true(got%equals(expected))
      call got%clear(); call expected%clear()
      !#
      got = split("  aa  ")
      call expected%append("aa")
      call assert_true(got%equals(expected))
      call got%clear(); call expected%clear()
      !#
      got = split("  aa  bb    cc     ")
      call expected%append("aa"); call expected%append("bb"); call expected%append("cc")
      call assert_true(got%equals(expected))
      call got%clear(); call expected%clear()
      !#
      got = split("       ")
      call assert_true(got%equals(expected))
      call got%clear(); call expected%clear()
      !#
      got = split(WHITESPACE)
      call assert_true(got%equals(expected))
      call got%clear(); call expected%clear()
      !#
      got = split("  aa  bb   "//TAB//"   cc     ")
      call expected%append("aa"); call expected%append("bb"); call expected%append("cc")
      call assert_true(got%equals(expected))
      call got%clear(); call expected%clear()
   end subroutine

   subroutine test_split_with_delimiter()
      type(StringBuffer) :: got
      type(StringBuffer) :: expected

      got = split("aa,bb", ",")
      call expected%append("aa"); call expected%append("bb")
      call assert_true(got%equals(expected))
      call got%clear(); call expected%clear()
      !#
      got = split("aa", ",")
      call expected%append("aa")
      call assert_true(got%equals(expected))
      call got%clear(); call expected%clear()
      !#
      got = split("aa,bb,cc", ",")
      call expected%append("aa"); call expected%append("bb"); call expected%append("cc")
      call assert_true(got%equals(expected))
      call got%clear(); call expected%clear()
      !#
      got = split(" aa,bb,cc ", ",")
      call expected%append(" aa"); call expected%append("bb"); call expected%append("cc ")
      call assert_true(got%equals(expected))
      call got%clear(); call expected%clear()
      !#
      got = split("aa bb cc", " ")
      call expected%append("aa"); call expected%append("bb"); call expected%append("cc")
      call assert_true(got%equals(expected))
      call got%clear(); call expected%clear()
      !#
      got = split(",aa,bb", ",")
      call expected%append(""); call expected%append("aa"); call expected%append("bb")
      call assert_true(got%equals(expected))
      call got%clear(); call expected%clear()
      !#
      got = split(",aa,bb,", ",")
      call expected%append("");
      call expected%append("aa"); call expected%append("bb")
      call expected%append("");
      call assert_true(got%equals(expected))
      call got%clear(); call expected%clear()
      !#
      got = split("aa bb", " ")
      call expected%append("aa"); call expected%append("bb")
      call assert_true(got%equals(expected))
      call got%clear(); call expected%clear()
      !#
      got = split(" aa bb", " ")
      call expected%append("");
      call expected%append("aa"); call expected%append("bb")
      call assert_true(got%equals(expected))
      call got%clear(); call expected%clear()
      !#
      got = split(" aa bb ", " ")
      call expected%append("");
      call expected%append("aa"); call expected%append("bb")
      call expected%append("");
      call assert_true(got%equals(expected))
      call got%clear(); call expected%clear()
      !#
      got = split(",aa,bb,", ",")
      call expected%append("");
      call expected%append("aa"); call expected%append("bb")
      call expected%append("");
      call assert_true(got%equals(expected))
      call got%clear(); call expected%clear()
      !#
      got = split("", ",")
      call expected%append("");
      call assert_true(got%equals(expected))
      call got%clear(); call expected%clear()
      !#
      got = split(",", ",")
      call expected%append(""); call expected%append("");
      call assert_true(got%equals(expected))
      call got%clear(); call expected%clear()
      !#
      got = split(",,", ",")
      call expected%append(""); call expected%append(""); call expected%append("")
      call assert_true(got%equals(expected))
      call got%clear(); call expected%clear()
      !#
      got = split("  a  b  c", " ")
      call expected%append(""); call expected%append("")
      call expected%append("a"); call expected%append("")
      call expected%append("b"); call expected%append("")
      call expected%append("c")
      call assert_true(got%equals(expected))
      call got%clear(); call expected%clear()
      !#
      got = split("a  b  c", " ")
      call expected%append("a"); call expected%append("")
      call expected%append("b"); call expected%append("")
      call expected%append("c")
      call assert_true(got%equals(expected))
      call got%clear(); call expected%clear()
      !#
      got = split("a  b  c  ", " ")
      call expected%append("a"); call expected%append("")
      call expected%append("b"); call expected%append("")
      call expected%append("c"); call expected%append(""); call expected%append("")
      call assert_true(got%equals(expected))
      call got%clear(); call expected%clear()
      !#
      got = split("user1;100;laci", ";")
      call expected%append("user1")
      call expected%append("100")
      call expected%append("laci")
      call assert_true(got%equals(expected))
      call got%clear(); call expected%clear()
      !#
      got = split("user1|||100|||laci", "|||")
      call expected%append("user1")
      call expected%append("100")
      call expected%append("laci")
      call assert_true(got%equals(expected))
      call got%clear(); call expected%clear()
      !#
      got = split("a,a", ",")
      call expected%append("a")
      call expected%append("a")
      call assert_true(got%equals(expected))
      call got%clear(); call expected%clear()
      !#
      got = split("  a  b  c  ", " ")
      call expected%append(""); call expected%append(""); call expected%append("a")
      call expected%append(""); call expected%append("b")
      call expected%append(""); call expected%append("c")
      call expected%append(""); call expected%append("")
      call assert_true(got%equals(expected))
      call got%clear(); call expected%clear()
      !#
      got = split("a b", " ")
      call expected%append("a")
      call expected%append("b")
      call assert_true(got%equals(expected))
      call got%clear(); call expected%clear()
      !#
      got = split("a  b", " ")
      call expected%append("a")
      call expected%append("")
      call expected%append("b")
      call assert_true(got%equals(expected))
      call got%clear(); call expected%clear()
      !#
      got = split("a,,b", ",")
      call expected%append("a")
      call expected%append("")
      call expected%append("b")
      call assert_true(got%equals(expected))
      call got%clear(); call expected%clear()
      !#
      got = split("a---b---c", "---")
      call expected%append("a")
      call expected%append("b")
      call expected%append("c")
      call assert_true(got%equals(expected))
      call got%clear(); call expected%clear()
      !#
      got = split("a---b--c", "---")
      call expected%append("a")
      call expected%append("b--c")
      call assert_true(got%equals(expected))
      call got%clear(); call expected%clear()
   end subroutine

end program
