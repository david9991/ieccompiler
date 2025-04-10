PROGRAM test_func_return
  VAR
    B : INT;
    C : INT;
    D : BOOL;
  END_VAR

  D := test_func_return_add(B,C);
END_PROGRAM

FUNCTION test_func_return_add : INT
  VAR 
    KK : INT;
    VV : BOOL;
  END_VAR

  VAR_INPUT
    BBB : INT;
    CCC : INT;
  END_VAR

  KK := BBB + CCC + 9;
  RETURN KK;
END_FUNCTION
