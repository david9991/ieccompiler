PROGRAM test_not
  VAR
    b_Start_Stop : DINT;
    N : DINT;
    bFlag : BOOL;
  END_VAR

  IF NOT bFlag THEN
    N := N + 7;
  ELSE
    N := N + 9;
  END_IF
END_PROGRAM