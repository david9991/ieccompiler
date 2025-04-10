PROGRAM test_priority
  VAR
    b_Start_Stop: DINT;
    N: DINT;
  END_VAR

  WHILE b_Start_Stop  > 200 OR b_Start_Stop < 111111 AND N < 222222 DO
    N := N + 10;
    N := b_Start_Stop+5-2*b_Start_Stop/N;      
  END_WHILE
END_PROGRAM
