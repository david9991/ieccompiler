PROGRAM test_paren_nest
  VAR
    A : INT;
  END_VAR
  A := A*(A/(A+ 2)+3);
  A := A*(A/(A+2) );//ok
  A := A*(A/(A+2));//Invalid syntax
  A := A*( (A+2)*3);//ok
  A := A*((A+2)*3);//Invalid syntax
END_PROGRAM