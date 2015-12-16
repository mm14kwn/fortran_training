PROGRAM AIDS
!Calculates number of accumulated AIDS cases in USA
INTEGER T	!year
REAL A	!number of cases

READ*, T
A=174.6*(T-1981.2)**3
PRINT*, 'Accumulated AIDS cases in US by year', T, ':', A
END PROGRAM AIDS