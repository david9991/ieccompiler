PROGRAM test_arithmetic
  VAR
    A : INT;
    B : INT;
    C : INT;
  END_VAR

  C := A + B - 9;
  C := A - B + 9;
  C := A * B / 2;
  C := A / 2 * B;
  C := B + 5 - 2 * B / C;
END_PROGRAM
