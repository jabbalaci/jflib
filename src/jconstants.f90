module jconstants

! Some useful constants.

   use jtypes, only: dp
   implicit none

   real(dp), parameter :: PI = acos(-1.0_dp)
   real(dp), parameter :: M_PI = PI

end module jconstants
