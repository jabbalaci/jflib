program example_jstring
   use jstring
   implicit none

   print '(*(g0))', 'capitalize("anna"): ', '"', capitalize("anna"), '"'
   print '(*(g0))', 'center("*", 3): ', "'", center("*", 3), "'"
   print '(*(g0))', 'count_elems("anna", "n"): ', count_elems("anna", "n")
   print '(*(g0))', 'endswith("01.png", ".png"): ', endswith("01.png", ".png")
   print '(*(g0))', 'equal_strings("*", "* "): ', equal_strings("*", "* ")
   print '(*(g0))', 'find("Shepard", "hep"): ', find("Shepard", "hep")
   print '(*(g0))', 'isascii("Éva"): ', isascii("Éva")
   print '(*(g0))', 'isdigit("2026"): ', isdigit("2026")
   print '(*(g0))', 'is_in("prog", "programming"): ', is_in("prog", "programming")
   print '(*(g0))', 'islower("anna"): ', islower("anna")
   print '(*(g0))', 'isupper("ANNA"): ', isupper("ANNA")
end program
