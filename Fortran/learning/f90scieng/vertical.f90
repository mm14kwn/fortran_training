PROGRAM Vertical
! Vertical motion under gravity

IMPLICIT NONE

REAL, PARAMETER :: G=9.8 ! acceleration due to gravity
REAL S ! displacement
REAL T ! time
REAL U ! initial speed

PRINT*, ' Time      Displacement'
PRINT*
U=60
T=6
S=U*T-G/2*T**2
PRINT*, T, S

END PROGRAM Vertical
