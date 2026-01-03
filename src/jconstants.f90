module jconstants

! Some useful constants.

   use jtypes, only: dp
   implicit none
   private

   real(dp), parameter :: PI = acos(-1.0_dp)
   real(dp), parameter :: M_PI = PI

   public :: PI, M_PI

end module jconstants
