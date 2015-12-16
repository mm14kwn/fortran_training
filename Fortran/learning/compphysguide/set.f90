program set
implicit none
real :: a, b
! Read in value of a
read(*,*) a
call setval(b)
write(*,*) b
contains
subroutine setval(x)
real :: x
x = a ! value of a is from main program
end subroutine setval
end program set