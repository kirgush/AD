(* Wolfram Language Test file *)

Test[ad[1.23, 5.67], ad[1.23, 5.67], TestID->"Test-20161118-P0Q2X8"]

Test[ad[x1, <|x1 -> y1|>] + 3, ad[3 + x1, <|x1 -> y1|>], TestID->"Test-20161118-Q8Y0I2"]

Test[-2 ad[x1, <|x1 -> y1|>], ad[-2 x1, <|x1 -> -2 y1|>]
	,
	TestID->"Test-20161118-A7H1S9"]

Test[ad[x1, <|x1 -> y1|>]^(-2), ad[1/x1^2, <|x1 -> -((2 y1)/x1^3)|>]
	,
	TestID->"Test-20161118-F3M2F6"]

Test[ad[Tan[x1], <|x1 -> y1|>] + ad[x2, <|x2 -> y2|>], ad[x2 + Tan[x1], <|x2 -> y2, x1 -> y1|>]
	,
	TestID->"Test-20161118-N5M7T8"]

Test[ad[x1, <|x1 -> y1|>] + ad[x2, <|x2 -> y2|>], ad[x1 + x2, <|x1 -> y1, x2 -> y2|>]
	,
	TestID->"Test-20161118-T6P3G2"]

Test[ad[x1, <|x1 -> y1|>] + 3 ad[x2, <|x2 -> y2|>], ad[x1 + 3 x2, <|x1 -> y1, x2 -> 3 y2|>]
	,
	TestID->"Test-20161118-D1X6E7"]

Test[ad[x1, <|x1 -> y1|>] ad[x2, <|x2 -> y2|>], ad[x1 x2, <|x1 -> x2 y1, x2 -> x1 y2|>]
	,
	TestID->"Test-20161118-G0J1L9"]

Test[Sin[ad[x1 x2, <|x1 -> x2 y1, x2 -> x1 y2|>]], ad[Sin[x1 x2], <|x1 -> x2 y1 Cos[x1 x2], x2 -> x1 y2 Cos[x1 x2]|>]
	,
	TestID->"Test-20161118-A7B0G4"]

Test[ad[x3, <|x3 -> y3|>] Sin[ad[x1 x2, <|x1 -> x2 y1, x2 -> x1 y2|>]], ad[x3 Sin[x1 x2], <|x3 -> y3 Sin[x1 x2], x1 -> x2 x3 y1 Cos[x1 x2],  x2 -> x1 x3 y2 Cos[x1 x2]|>]
	,
	TestID->"Test-20161118-N4K0D1"]

Test[Exp[ad[x3, <|x3 -> y3|>] Sin[ad[x1 x2, <|x1 -> x2 y1, x2 -> x1 y2|>]]], ad[E^(x3 Sin[x1 x2]), <|x3 -> E^(x3 Sin[x1 x2]) y3 Sin[x1 x2], x1 -> E^(x3 Sin[x1 x2]) x2 x3 y1 Cos[x1 x2], x2 -> E^(x3 Sin[x1 x2]) x1 x3 y2 Cos[x1 x2]|>]
	,
	TestID->"Test-20161118-D9U3R2"]

Test[Exp[ad[x2, <|x2 -> y2|>] Sin[ad[x1 x2, <|x1 -> x2 y1, x2 -> x1 y2|>]]], ad[E^(x2 Sin[x1 x2]), <|x2 -> E^(x2 Sin[x1 x2]) (x1 x2 y2 Cos[x1 x2] + y2 Sin[x1 x2]), x1 -> E^(x2 Sin[x1 x2]) x2^2 y1 Cos[x1 x2]|>]
	,
	TestID->"Test-20161118-D2I1F5"] 

Test[Exp[ad[x3, <|x3 -> y3|>] Sin[ad[x1, <|x1 -> y1|>] ad[x2, <|x2 -> y2|>]]], ad[E^(x3 Sin[x1 x2]), <|x3 -> E^(x3 Sin[x1 x2]) y3 Sin[x1 x2], x1 -> E^(x3 Sin[x1 x2]) x2 x3 y1 Cos[x1 x2], x2 -> E^(x3 Sin[x1 x2]) x1 x3 y2 Cos[x1 x2]|>]
	,
	TestID->"Test-20161118-I0S0A7"]

Test[Sin[ad[x1, <|x1 -> y1|>] ad[x2, <|x2 -> y2|>]] Cos[ad[x3, <|x3 -> y3|>] ad[x4, <|x4 -> y4|>]^3] (Log[Tan[ad[x5, <|x5 -> y5|>]/ad[x6, <|x6 -> y6|>]]])^(-2), ad[(Cos[x3 x4^3] Sin[x1 x2])/Log[Tan[x5/x6]]^2, <|x3 -> -((x4^3 y3 Sin[x1 x2] Sin[x3 x4^3])/Log[Tan[x5/x6]]^2), x4 -> -((3 x3 x4^2 y4 Sin[x1 x2] Sin[x3 x4^3])/Log[Tan[x5/x6]]^2), x5 -> -((2 y5 Cos[x3 x4^3] Csc[x5/x6] Sec[x5/x6] Sin[x1 x2])/(x6 Log[Tan[x5/x6]]^3)), x6 -> (2 x5 y6 Cos[x3 x4^3] Csc[x5/x6] Sec[x5/x6] Sin[x1 x2])/(x6^2 Log[Tan[x5/x6]]^3), x1 -> (x2 y1 Cos[x1 x2] Cos[x3 x4^3])/Log[Tan[x5/x6]]^2,  x2 -> (x1 y2 Cos[x1 x2] Cos[x3 x4^3])/Log[Tan[x5/x6]]^2|>]
	,
	TestID->"Test-20161118-Y1D5O4"]
	
Test[ad[x1, <|x1 -> y1|>]^ad[x2, <|x2 -> y2|>], ad[x1^x2, <|x1 -> x1^(-1 + x2) x2 y1, x2 -> x1^x2 y2 Log[x1]|>]
	,
	TestID->"Test-20161118-E6B3R4"]
	
Test[Sin[ad[x + I y, <|x1 -> y1, x2 -> y2|>]], ad[Sin[x + I y], <|x1 -> y1 Cos[x + I y], x2 -> y2 Cos[x + I y]|>]
	,
	TestID->"Test-20161118-F4V1V7"]

Test[{ad[1.23, <|x1 -> y1|>] < ad[4.56, <|x2 -> y2|>],
	ad[1.23, <|x1 -> y1|>] <= ad[4.56, <|x2 -> y2|>],
  ad[1.23, <|x1 -> y1|>] > ad[4.56, <|x2 -> y2|>],
  ad[1.23, <|x1 -> y1|>] >= ad[4.56, <|x2 -> y2|>]}, {True, True, False, False}
	,
	TestID->"Test-20161215-U5J0S6"]