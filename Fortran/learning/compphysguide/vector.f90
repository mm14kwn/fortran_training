program vector
implicit none
real :: v(3)
real :: x
integer :: i
v(1) = 0.25
v(2) = 1.2
v(3) = 0.2
! compute the modulus squared of the vector
x = 0.0
do i=1,3
x = x + v(i)*v(i)
end do
write(*,*) 'Modulus squared = ',x
end program vector
