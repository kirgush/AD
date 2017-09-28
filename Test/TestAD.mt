(* Wolfram Language Test file *)

Test[fd[1.23, 5.67], fd[1.23, 5.67], TestID -> "Test-20161118-P0Q2X8"];

Test[toValue[fd[3 + x1, <|x1 -> y1|>]], 3 + x1
	,
	TestID->"TestAD-20170928-G2I0C4"]

Test[toSensitivity[fd[3 + x1, <|x1 -> y1, t2->u2|>]], <|x1 -> y1, t2->u2|>
	,
	TestID->"TestAD-20170928-Q2H5O5"]

Test[fd[x1, <|x1 -> y1|>] + 3, fd[3 + x1, <|x1 -> y1|>], TestID -> "Test-20161118-Q8Y0I2"];

Test[-2 fd[x1, <|x1 -> y1|>], fd[-2 x1, <|x1 -> -2 y1|>]
  ,
  TestID -> "Test-20161118-A7H1S9"];

Test[fd[x1, <|x1 -> y1|>]^(-2), fd[1 / x1^2, <|x1 -> -((2 y1) / x1^3)|>]
  ,
  TestID -> "Test-20161118-F3M2F6"];

Test[fd[Tan[x1], <|x1 -> y1|>] + fd[x2, <|x2 -> y2|>], fd[x2 + Tan[x1], <|x2 -> y2, x1 -> y1|>]
  ,
  TestID -> "Test-20161118-N5M7T8"];

Test[fd[x1, <|x1 -> y1|>] + fd[x2, <|x2 -> y2|>], fd[x1 + x2, <|x1 -> y1, x2 -> y2|>]
  ,
  TestID -> "Test-20161118-T6P3G2"];

Test[fd[x1, <|x1 -> y1|>] + 3 fd[x2, <|x2 -> y2|>], fd[x1 + 3 x2, <|x1 -> y1, x2 -> 3 y2|>]
  ,
  TestID -> "Test-20161118-D1X6E7"];

Test[fd[x1, <|x1 -> y1|>] fd[x2, <|x2 -> y2|>], fd[x1 x2, <|x1 -> x2 y1, x2 -> x1 y2|>]
  ,
  TestID -> "Test-20161118-G0J1L9"];

Test[Sin[fd[x1 x2, <|x1 -> x2 y1, x2 -> x1 y2|>]], fd[Sin[x1 x2], <|x1 -> x2 y1 Cos[x1 x2], x2 -> x1 y2 Cos[x1 x2]|>]
  ,
  TestID -> "Test-20161118-A7B0G4"];

Test[fd[x3, <|x3 -> y3|>] Sin[fd[x1 x2, <|x1 -> x2 y1, x2 -> x1 y2|>]], fd[x3 Sin[x1 x2], <|x3 -> y3 Sin[x1 x2], x1 -> x2 x3 y1 Cos[x1 x2], x2 -> x1 x3 y2 Cos[x1 x2]|>]
  ,
  TestID -> "Test-20161118-N4K0D1"];

Test[Exp[fd[x3, <|x3 -> y3|>] Sin[fd[x1 x2, <|x1 -> x2 y1, x2 -> x1 y2|>]]], fd[E^(x3 Sin[x1 x2]), <|x3 -> E^(x3 Sin[x1 x2]) y3 Sin[x1 x2], x1 -> E^(x3 Sin[x1 x2]) x2 x3 y1 Cos[x1 x2], x2 -> E^(x3 Sin[x1 x2]) x1 x3 y2 Cos[x1 x2]|>]
  ,
  TestID -> "Test-20161118-D9U3R2"];

Test[Exp[fd[x2, <|x2 -> y2|>] Sin[fd[x1 x2, <|x1 -> x2 y1, x2 -> x1 y2|>]]], fd[E^(x2 Sin[x1 x2]), <|x2 -> E^(x2 Sin[x1 x2]) (x1 x2 y2 Cos[x1 x2] + y2 Sin[x1 x2]), x1 -> E^(x2 Sin[x1 x2]) x2^2 y1 Cos[x1 x2]|>]
  ,
  TestID -> "Test-20161118-D2I1F5"];

Test[Exp[fd[x3, <|x3 -> y3|>] Sin[fd[x1, <|x1 -> y1|>] fd[x2, <|x2 -> y2|>]]], fd[E^(x3 Sin[x1 x2]), <|x3 -> E^(x3 Sin[x1 x2]) y3 Sin[x1 x2], x1 -> E^(x3 Sin[x1 x2]) x2 x3 y1 Cos[x1 x2], x2 -> E^(x3 Sin[x1 x2]) x1 x3 y2 Cos[x1 x2]|>]
  ,
  TestID -> "Test-20161118-I0S0A7"];

Test[Sin[fd[x1, <|x1 -> y1|>] fd[x2, <|x2 -> y2|>]] Cos[fd[x3, <|x3 -> y3|>] fd[x4, <|x4 -> y4|>]^3] (Log[Tan[fd[x5, <|x5 -> y5|>] / fd[x6, <|x6 -> y6|>]]])^(-2), fd[(Cos[x3 x4^3] Sin[x1 x2]) / Log[Tan[x5 / x6]]^2, <|x3 -> -((x4^3 y3 Sin[x1 x2] Sin[x3 x4^3]) / Log[Tan[x5 / x6]]^2), x4 -> -((3 x3 x4^2 y4 Sin[x1 x2] Sin[x3 x4^3]) / Log[Tan[x5 / x6]]^2), x5 -> -((2 y5 Cos[x3 x4^3] Csc[x5 / x6] Sec[x5 / x6] Sin[x1 x2]) / (x6 Log[Tan[x5 / x6]]^3)), x6 -> (2 x5 y6 Cos[x3 x4^3] Csc[x5 / x6] Sec[x5 / x6] Sin[x1 x2]) / (x6^2 Log[Tan[x5 / x6]]^3), x1 -> (x2 y1 Cos[x1 x2] Cos[x3 x4^3]) / Log[Tan[x5 / x6]]^2, x2 -> (x1 y2 Cos[x1 x2] Cos[x3 x4^3]) / Log[Tan[x5 / x6]]^2|>]
  ,
  TestID -> "Test-20161118-Y1D5O4"];

Test[fd[x1, <|x1 -> y1|>]^fd[x2, <|x2 -> y2|>], fd[x1^x2, <|x1 -> x1^(-1 + x2) x2 y1, x2 -> x1^x2 y2 Log[x1]|>]
  ,
  TestID -> "Test-20161118-E6B3R4"];

Test[Sin[fd[x + I y, <|x1 -> y1, x2 -> y2|>]], fd[Sin[x + I y], <|x1 -> y1 Cos[x + I y], x2 -> y2 Cos[x + I y]|>]
  ,
  TestID -> "Test-20161118-F4V1V7"];

Test[{fd[1.23, <|x1 -> y1|>] < fd[4.56, <|x2 -> y2|>],
  fd[1.23, <|x1 -> y1|>] <= fd[4.56, <|x2 -> y2|>],
  fd[1.23, <|x1 -> y1|>] > fd[4.56, <|x2 -> y2|>],
  fd[1.23, <|x1 -> y1|>] >= fd[4.56, <|x2 -> y2|>]}, {True, True, False, False}
  ,
  TestID -> "Test-20161215-U5J0S6"];