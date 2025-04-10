PROGRAM test_repeat
  VAR
    i  : INT;
  END_VAR

  REPEAT
    i := i + 1;
  UNTIL i = 4;
  END_REPEAT
END_PROGRAM