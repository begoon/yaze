Program Savage;
(* see Byte, Oct '87, p. 277 *)

Const
  ILOOP = 500;

VAR
  I: Integer;
  A: Real;

Function Tan(X: Real): Real;
Begin
  Tan := Sin(X)/Cos(X);
End;

Begin
  A := 1.0;
  For I := 1 to ILOOP do
    A := Tan(Arctan(Exp(Ln(Sqrt(A*A)))))+1.0;
  Writeln('A = ', A:20:14);
End.
