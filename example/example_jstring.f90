program example_jstring
   use jstring
   use jstringbuffer, only: StringBuffer
   implicit none
   type(StringBuffer) :: sb

   print '(*(g0))', 'capitalize("anna"): ', '"', capitalize("anna"), '"'
   print '(*(g0))', 'center("*", 3): ', "'", center("*", 3), "'"
   print '(*(g0))', 'chomp("line\n"): ', "'", chomp("line"//LF), "'"
   print '(*(g0))', 'count_elems("anna", "n"): ', count_elems("anna", "n")
   print '(*(g0))', 'endswith("01.png", ".png"): ', endswith("01.png", ".png")
   print '(*(g0))', 'equal_strings("*", "* "): ', equal_strings("*", "* ")
   print '(*(g0))', 'find("Fortran", "r"): ', find("Fortran", "r")
   print '(*(g0))', 'isascii("Éva"): ', isascii("Éva")
   print '(*(g0))', 'isdigit("2026"): ', isdigit("2026")
   print '(*(g0))', 'is_in("prog", "programming"): ', is_in("prog", "programming")
   print '(*(g0))', 'islower("anna"): ', islower("anna")
   print '(*(g0))', 'isspace(" \t \r\n"): ', isspace(" "//TAB//" "//CR//LF)
   print '(*(g0))', 'isupper("ANNA"): ', isupper("ANNA")
   print '(*(g0))', 'lower("ANNA"): ', '"', lower("ANNA"), '"'
   print '(*(g0))', 'lstrip(" \t anna  "): ', '"', lstrip(" "//TAB//" anna  "), '"'
   print '(*(g0))', 'removeprefix("01.jpg", "01"): ', '"', removeprefix("01.jpg", "01"), '"'
   print '(*(g0))', 'removesuffix("01.jpg", ".jpg"): ', '"', removesuffix("01.jpg", ".jpg"), '"'
   print '(*(g0))', 'replace("cat dog cat", "cat", "kitten"): ', '"', replace("cat dog cat", "cat", "kitten"), '"'
   print '(*(g0))', 'rev("Fortran"): ', '"', rev("Fortran"), '"'
   print '(*(g0))', 'rfind("Fortran", "r"): ', rfind("Fortran", "r")
   print '(*(g0))', 'rstrip("  anna  \n"): ', '"', rstrip("  anna  "//LF), '"'
   print '(*(g0))', 'slice("programming", 1, 4): ', '"', slice("programming", 1, 4), '"'
   print '(*(g0))', 'slice("programming", 4, 1, -1): ', '"', slice("programming", 4, 1, -1), '"'
   print '(*(g0))', 'split("  aa  bb  cc  "):'
   sb = split("  aa  bb  cc  ")
   call sb%debug()
   print '(*(g0))', 'split("aa;bb;cc", ";"):'
   sb = split("aa;bb;cc", ";")
   call sb%debug()
   print '(*(g0))', 'startswith("01.png", "01"): ', startswith("01.png", "01")
   print '(*(g0))', 'strip(" \t anna \t \n"): ', '"', strip(" "//TAB//" anna "//TAB//" "//LF), '"'
   print '(*(g0))', 'swapcase("fORTRAN"): ', '"', swapcase("fORTRAN"), '"'
   print '(*(g0))', 'upper("anna"): ', '"', upper("anna"), '"'
   print '(*(g0))', 'zfill("7", 3): ', '"', zfill("7", 3), '"'
   print '(a)', "---"
end program
