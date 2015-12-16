program testset
! include global data
use gd
! make available the setval routine
use setval
real :: x, y
! read values from terminal
read(*,*) a_g, b_g
! set x and y and check output
call setv(x, y)
write(*,*) x, y
end program testset