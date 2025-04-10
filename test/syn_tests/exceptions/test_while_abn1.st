PROGRAM test_while_abn1
  VAR
    b_Start_Stop:DINT;
    N:DINT;
  END_VAR

  WHILE b_Start_Stop > 111111 OR N < 222222
    N := N + 7;
  END_WHILE
END_PROGRAM
