PROGRAM TWODTIDE
IMPLICIT NONE

 
  INTEGER, PARAMETER :: real_8_30 = SELECTED_REAL_KIND(P=8, R=30)
  REAL(KIND=real_8_30), PARAMETER :: Pi=3.1415927  
  INTEGER :: N, M, I, J, in1, in2, in3, in4, k
  REAL(KIND=real_8_30) :: dx, dy, dt, tau, fprime, tidal_period, amp, T, a1, a2, a3, a4, phase1, phase2, time
  REAL(KIND=real_8_30), DIMENSION(:), ALLOCATABLE :: a
  REAL(KIND=real_8_30), DIMENSION(:,:), ALLOCATABLE :: u, v, vf, px, py, eta, ux, uf, kap
  LOGICAL :: INL
  PRINT*, 'INPUT N=xrange'
  READ*, N
  PRINT*, 'INPUT M=yrange'
  READ*, M
  dx=1/REAL(N)
PRINT*,dx
  dy=dx
  dt=0.001
  PRINT*, 'INPUT TIDAL AMPLITUDE'
  READ*, amp
  PRINT*, 'INPUT TIDAL PERIOD'
  READ*, tidal_period
  PRINT*, 'INPUT MAX TIME'
  READ*, T
  PRINT*, 'INPUT tau'
  READ*, tau
  PRINT*, 'INPUT fprime'
  READ*, fprime
  PRINT*, 'INLETS? T/F'
  READ*, inl
  ALLOCATE(a(1:M))
  ALLOCATE(u(1:M+1,1:N+1))
  ALLOCATE(v(1:M+1,1:N+1))
  ALLOCATE(eta(1:M,1:N))
  ALLOCATE(px(1:M,1:N))
  ALLOCATE(py(1:M,1:N))
!!$  ALLOCATE(kap(1:1+NINT(T/dt),1:N))
  ALLOCATE(uf(1:M,1:N))
  ALLOCATE(vf(1:M,1:N))
  a=0
  u=0
  v=0
  eta=0
  k=0
  IF (INL) THEN
  PRINT*, 'lower edge of inlet 1 (proportion of M) = '
  READ*, a1
  PRINT*, 'upper edge of inlet 1 (proportion of M) = '
  READ*, a2
  PRINT*, 'lower edge of inlet 2 (proportion of M) = '
  READ*, a3
  PRINT*, 'upper edge of inlet 2 (proportion of M) = '
  READ*, a4
  PRINT*, 'Phase shift of inlet 1 = '
  READ*, phase1
  PRINT*, 'Phase shift of inlet 2 = '
  READ*, phase2
  in1=NINT(a1*M)
  in2=NINT(a2*M)
  in3=NINT(a3*M)
  in4=NINT(a4*M)
  DO I=in1,in2
     a(I)=amp
  END DO
  DO I=in3,in4
     a(I)=amp
  END DO
ELSE
  a1=0
  a2=1
  a3=0
  a4=0
  phase1=0
  phase2=0
  a=amp
END IF
time=0
DO WHILE (time .LT. T)

   DO J=1,M
      DO I=2,N
         px(J,I)=(eta(J,I)-eta(J,I-1))/dx
         vf(J,I)=(v(J,I)+v(J+1,I)+v(J,I-1)+v(J+1,I-1))/4
         u(J,I)=u(J,I)+dt*(-px(J,I)+fprime*vf(J,I)-u(J,I)/tau)
      END DO
    END DO

    IF (INL) THEN
    DO J=in1,in2
       px(J,1)=(eta(J,1)-a(J)*SIN((2*pi*time/tidal_period)+phase1))/dx
    END DO
    DO J=in3,in4
       px(J,1)=(eta(J,1)-a(J)*SIN((2*pi*time/tidal_period)+phase2))/dx
    END DO
    ELSE
       DO J=1,M
          px(J,1)=(eta(J,1)-a(J)*SIN(2*pi*time/tidal_period))/dx
       END DO
    END IF
    DO J=1,M
       vf(J,1)=(v(J,1)+v(J+1,1))/2
       u(J,1)=u(J,1)+dt*(-px(J,1)-u(j,1)/tau)
    END DO
    DO I=2,N
       DO J=2,M
          py(J,I)=(eta(J,I)-eta(J-1,I))/dy
          uf(J,I)=(u(J,I)+u(J,I+1)+u(J-1,I)+u(J-1,I+1))/4
          v(J,I)=v(J,I)+dt*(-py(J,I)-fprime*uf(J,I)-v(J,I)/tau)
       END DO
    END DO
    DO J=1,M
       DO I=1,N
          eta(J,I)=eta(J,I)-dt*((u(J,I+1)-u(J,I))/dx +(v(J+1,I)-v(J,I))/dy)
       END DO
    END DO
    DO I=1,N+1
       u(M+1,I)=0
    END DO
    DO J=1,M+1
       u(J,N+1)=0
    END DO
!!$    DO I=1,N
!!$       kap(k,I)=eta(1,I)
!!$    END DO
    k=k+1
    time=time+dt
 END DO

!!$open(unit=8,file='uout.dat',& ! Unformatted file, stream access
!!$  form='unformatted',access='stream')
!!$write(unit=8) u         ! Write array
!!$close(unit=8)
!!$
!!$open(unit=8,file='vout.dat',& ! Unformatted file, stream access
!!$  form='unformatted',access='stream')
!!$write(unit=8) v          ! Write array
!!$close(unit=8)
!!$
!!$open(unit=8,file='etaout.dat',& ! Unformatted file, stream access
!!$  form='unformatted',access='stream')
!!$write(unit=8) eta          ! Write array
!!$close(unit=8)
!!$
!!$open(unit=8,file='kapout.dat',& ! Unformatted file, stream access
!!$  form='unformatted',access='stream')
!!$write(unit=8) kap          ! Write array
!!$close(unit=8)
open(1,file='uout.dat',status='new')
WRITE(1,*) u
close(1)
open(1,file='vout.dat',status='new')
WRITE(1,*) v
close(1)
open(1,file='etaout.dat',status='new')
WRITE(1,*) eta
close(1)
!!$open(1,file='kapout.dat',status='new')
!!$WRITE(1,*) kap
!!$close(1)
END PROGRAM
