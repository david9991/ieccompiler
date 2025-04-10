PROGRAM test_parenno
  VAR
    b_Start_Stop: DINT;
    N: DINT;
  END_VAR

  IF b_Start_Stop > 111111 AND  b_Start_Stop < 222222 OR  b_Start_Stop < 333000 THEN
    N := N + 7;
  ELSIF b_Start_Stop > 333333 OR b_Start_Stop < 111111 AND  b_Start_Stop > 5 THEN
    N := N + 8;
  ELSE 
    N := N + 9;
  END_IF
END_PROGRAM
