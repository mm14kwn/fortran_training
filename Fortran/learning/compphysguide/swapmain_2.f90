program swapmain
use swapmod ! use statements must come first
implicit none
real :: a, b
! Read in two values
read(*,*) a, b
call swap(a,b)
write(*,*) a, b
end program swapmain