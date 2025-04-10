PROGRAM test_fun_args
  VAR
    B:INT;
    C:INT;
    D:BOOL;
  END_VAR

  C:=test_fun_args_add(B);
END_PROGRAM


FUNCTION test_fun_args_add:INT
  VAR 
    KK:INT;
    VV:BOOL;
  END_VAR

  VAR_INPUT
    BBB:INT;
    CCC:INT;
  END_VAR

  KK := BBB + CCC + 9;
END_FUNCTION
