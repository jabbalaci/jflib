program example_jstring
   use jstring
   implicit none

   print *, isupper("A")
   print *, isupper("a")
   print *, islower("A")
   print *, islower("a")
   print *, upper("laci")
   print *, upper("Kiss Tamas")
   print *, upper("a")
   print *, lower("AbCdE")
   print *, lower("XXX")
   print *, lower("X")
   print *, islower("ablak ")
   print *, isupper("Rf")
end program
