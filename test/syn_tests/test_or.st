PROGRAM test_or
  VAR
    b_Start_Stop : DINT;
    N : DINT;
  END_VAR

  IF b_Start_Stop > 111111 OR N < 222222 THEN
    N := N + 7;
  ELSIF b_Start_Stop > 333333 THEN
    N := N + 8;
  ELSE
    N := N + 9;
  END_IF
END_PROGRAM
