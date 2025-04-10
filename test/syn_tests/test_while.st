PROGRAM test_while
  VAR
    b_Start_Stop: DINT;
    N: DINT;
  END_VAR

  WHILE b_Start_Stop  > 200 AND b_Start_Stop < 111111 OR N < 222222 DO
    N := N + 10;
  END_WHILE
END_PROGRAM
