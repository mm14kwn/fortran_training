PROGRAM SEICHE
IMPLICIT NONE

REAL, PARAMETER :: Pi=3.1415927
INTEGER, PARAMETER :: real_8_30 = SELECTED_REAL_KIND(P=8, R=30)
INTEGER :: k, num, I, N
REAL(KIND=real_8_30) :: dx, dt, amp, tau, tidal_period, time, T
REAL(KIND=real_8_30), DIMENSION(:), ALLOCATABLE :: px, eta, ux, u, etaend
!REAL :: u(N+1,1)
!REAL :: etaend(num,1)
PRINT*, 'INPUT N'
READ*, N

PRINT*, 'INPUT TIDAL AMPLITUDE'
READ*, amp
PRINT*, 'INPUT TIDAL PERIOD'
READ*, tidal_period
PRINT*, 'INPUT MAX TIME'
READ*, T
dx=1/(REAL(N))
udt=0.1*dx
num=NINT(T/dt)
PRINT*, T, dt, num, N, dx, REAL(N)
time=0
k=0
ALLOCATE(px(1:N))
ALLOCATE(eta(1:N))
ALLOCATE(ux(1:N))
ALLOCATE(u(1:N+1))
ALLOCATE(etaend(1:num+1))
eta=0
u=0
PRINT*, 'INPUT LINEAR DAMPING TIME'
READ*, tau

DO WHILE (time<T)
   DO I=2,N
      px(I)=(eta(I)-(eta(I-1)/dx))
   END DO
   px(1)=(eta(1)-amp*SIN(2*Pi*t/tidal_period))/dx
   DO I=1,N
      u(I)=u(I)+dt*(-px(I)-(u(I)/tau))
   END DO
   DO I=1,N
      ux(I)=(u(I+1)-u(I))/dx
      eta(I)=eta(I)-(dt*ux(I))
   END DO
   time=time+dt
   k=k+1
   etaend(k)=eta(N)
!   PRINT*, k, SIZE(etaend), num
END DO
PRINT*, 'Run Complete - Writing to file'
open(1, file='./etaend.dat', status='new')
do I=1,k-1
!PRINT*, I
write(1,*) etaend(I)
end do
PRINT*, 'Written to file etaend.dat'
END PROGRAM SEICHE
