PROGRAM MONEY
!Calculates balance after interest compounded
  REAL BALANCE, INTEREST, RATE

  BALANCE=1000
  RATE=0.09
  INTEREST=RATE*BALANCE
  BALANCE=BALANCE+INTEREST
  PRINT*, 'New Balance:', BALANCE
END PROGRAM MONEY