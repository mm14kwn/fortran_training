PROGRAM BOND
! Calculate bond repayment
  IMPLICIT NONE
  REAL r, L, P, Na, Nb, N
  r = 0.15
  L = 50000
  P = 800
  Na = LOG(P/(P-(r*L/12)))
  Nb = 12*LOG(1+(r/12))
  N=Na/Nb
  PRINT*, 'Years to repay = ', N
END PROGRAM BOND
