PROGRAM test_while_abn4
  VAR
    b_Start_Stop:DINT;
    N:DINT;
  END_VAR

  WHILE b_Start_Stop > 111111 OR N < 222222 DO
    N := N + 7;
  END_WHILE

  WHILE b_Start_Stop > 111111 AND N < 222222 DO
    N := N + 8;
  END_WHILE

  WHILE b_Start_Stop < 100 OR (b_Start_Stop > 111111 AND b_Start_Stop < 222222)  DO
    N := N + 9;
  END_WHILE

  WHILE b_Start_Stop  > 200 AND (b_Start_Stop < 111111 OR N < 222222 ) DO
    N := N + 10;
  END_WHILE
END_PROGRAM
