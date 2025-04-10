FUNCTION_BLOCK test_fb
  VAR
    b_Start_Stop:DINT;
    N : DINT := 10;
  END_VAR

  IF b_Start_Stop > 0 THEN
    N := N + 7;
  ELSIF b_Start_Stop > 111111 
    AND N < 222222
    AND (N > 333333 AND N < 444444)
    AND N <> 555555 THEN
    N := N + 8;
  ELSE
    N := N + 9;
  END_IF
END_FUNCTION_BLOCK