PROGRAM test_fb_call
  VAR
    fb : TestFb;
  END_VAR
  fb(TRUE, 12, 13);

END_PROGRAM

FUNCTION_BLOCK test_fb_define
  VAR_INPUT
    bFlag:BOOL;
    A:INT;
    B:INT;
  END_VAR
END_FUNCTION_BLOCK
