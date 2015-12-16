program nagdet
! we make available the chapter f module of nag
use nag_f77_f_chapter
implicit none
! variables used in the program
real(kind(1.0d0)) :: m(3,3), d, wrk(2)
integer i, n, ifail
! assign values to only the upper 2x2 portion of
! the 3x3 array
m(1,1)=2 ; m(1,2)=0
m(2,1)=0 ; m(2,2)=2
! set up input values and call the NAG routine
! note the difference between i and n values
i=3 ; n=2 ; ifail=0
call f03aaf(m,i,n,d,wrk,ifail)
if (ifail == 0) then
write(*,*) 'Determinant is ',d
else
write(*,*) 'F03AAF error:',ifail
end if
end program nagdet
