program choice
implicit none
character(len=1) :: x
write(*,*) 'Select a or b'
read(*,*) x
select case(x)
case('A','a')
write(*,*) 'A selected'
case('B','b')
write(*,*) 'B selected'
case default
write(*,*) 'Error: unknown option'
end select
end program choice
