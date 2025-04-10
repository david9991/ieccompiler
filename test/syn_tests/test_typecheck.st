PROGRAM test_typecheckend_type
  VAR
    B: INT;
    C: INT;
    D: BOOL;
  END_VAR

  C := functest(D, B);
END_PROGRAM

FUNCTION functest:INT
  VAR 
    KK: INT;
  END_VAR
  VAR_INPUT
    bFlag: BOOL;
    nIndex: INT;
  END_VAR
  
  IF bFlag <> 0 THEN
    KK := nIndex + 1;
  END_IF
END_FUNCTION

