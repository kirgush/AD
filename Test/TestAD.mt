(* Wolfram Language Test file *)

Test[
    fd[1.23, 5.67]
    ,
    fd[1.23, 5.67]
    ,
    TestID -> "Test-20161118-P0Q2X8"
];

Test[
    toValue[fd[3 + x1, <|x1 -> y1|>]]
    ,
    3 + x1
    ,
    TestID->"TestAD-20170928-G2I0C4"
]

Test[
    toSensitivity[fd[3 + x1, <|x1 -> y1, t2->u2|>]]
    ,
    <|x1 -> y1, t2->u2|>
    ,
    TestID->"TestAD-20170928-Q2H5O5"
]

Test[
    fd[x1, <|x1 -> y1|>] + 3
    ,
    fd[3 + x1, <|x1 -> y1|>]
    ,
    TestID -> "Test-20161118-Q8Y0I2"
];

Test[
    -2 fd[x1, <|x1 -> y1|>]
    ,
    fd[-2 x1, <|x1 -> -2 y1|>]
    ,
    TestID -> "Test-20161118-A7H1S9"
];

Test[
    fd[x1, <|x1 -> y1|>]^(-2)
    ,
    fd[1 / x1^2, <|x1 -> -((2 y1) / x1^3)|>]
    ,
    TestID -> "Test-20161118-F3M2F6"
];

Test[
    fd[Tan[x1], <|x1 -> y1|>] + fd[x2, <|x2 -> y2|>]
    ,
    fd[x2 + Tan[x1], <|x2 -> y2, x1 -> y1|>]
    ,
    TestID -> "Test-20161118-N5M7T8"
];

Test[
    fd[x1, <|x1 -> y1|>] + fd[x2, <|x2 -> y2|>]
    ,
    fd[x1 + x2, <|x1 -> y1, x2 -> y2|>]
    ,
    TestID -> "Test-20161118-T6P3G2"
];

Test[
    fd[x1, <|x1 -> y1|>] + 3 fd[x2, <|x2 -> y2|>]
    ,
    fd[x1 + 3 x2, <|x1 -> y1, x2 -> 3 y2|>]
    ,
    TestID -> "Test-20161118-D1X6E7"
];

Test[
    fd[x1, <|x1 -> y1|>] fd[x2, <|x2 -> y2|>]
    ,
    fd[x1 x2, <|x1 -> x2 y1, x2 -> x1 y2|>]
    ,
    TestID -> "Test-20161118-G0J1L9"
];

Test[
    Sin[fd[x1 x2, <|x1 -> x2 y1, x2 -> x1 y2|>]]
    ,
    fd[Sin[x1 x2], <|x1 -> x2 y1 Cos[x1 x2], x2 -> x1 y2 Cos[x1 x2]|>]
    ,
    TestID -> "Test-20161118-A7B0G4"
];

Test[
    fd[x3, <|x3 -> y3|>] Sin[fd[x1 x2, <|x1 -> x2 y1, x2 -> x1 y2|>]]
    ,
    fd[x3 Sin[x1 x2], <|x3 -> y3 Sin[x1 x2], x1 -> x2 x3 y1 Cos[x1 x2], x2 -> x1 x3 y2 Cos[x1 x2]|>]
    ,
    TestID -> "Test-20161118-N4K0D1"
];

Test[
    Exp[fd[x3, <|x3 -> y3|>] Sin[fd[x1 x2, <|x1 -> x2 y1, x2 -> x1 y2|>]]]
    ,
    fd[E^(x3 Sin[x1 x2]), <|x3 -> E^(x3 Sin[x1 x2]) y3 Sin[x1 x2], x1 -> E^(x3 Sin[x1 x2]) x2 x3 y1 Cos[x1 x2], x2 -> E^(x3 Sin[x1 x2]) x1 x3 y2 Cos[x1 x2]|>]
    ,
    TestID -> "Test-20161118-D9U3R2"
];

Test[
    Exp[fd[x2, <|x2 -> y2|>] Sin[fd[x1 x2, <|x1 -> x2 y1, x2 -> x1 y2|>]]]
    ,
    fd[E^(x2 Sin[x1 x2]), <|x2 -> E^(x2 Sin[x1 x2]) (x1 x2 y2 Cos[x1 x2] + y2 Sin[x1 x2]), x1 -> E^(x2 Sin[x1 x2]) x2^2 y1 Cos[x1 x2]|>]
    ,
    TestID -> "Test-20161118-D2I1F5"
];

Test[
    Exp[fd[x3, <|x3 -> y3|>] Sin[fd[x1, <|x1 -> y1|>] fd[x2, <|x2 -> y2|>]]]
    ,
    fd[E^(x3 Sin[x1 x2]), <|x3 -> E^(x3 Sin[x1 x2]) y3 Sin[x1 x2], x1 -> E^(x3 Sin[x1 x2]) x2 x3 y1 Cos[x1 x2], x2 -> E^(x3 Sin[x1 x2]) x1 x3 y2 Cos[x1 x2]|>]
    ,
    TestID -> "Test-20161118-I0S0A7"
];

Test[
    Sin[fd[x1, <|x1 -> y1|>] fd[x2, <|x2 -> y2|>]] Cos[fd[x3, <|x3 -> y3|>] fd[x4, <|x4 -> y4|>]^3] (Log[Tan[fd[x5, <|x5 -> y5|>] / fd[x6, <|x6 -> y6|>]]])^(-2)
    ,
    fd[(Cos[x3 x4^3] Sin[x1 x2]) / Log[Tan[x5 / x6]]^2, <|x3 -> -((x4^3 y3 Sin[x1 x2] Sin[x3 x4^3]) / Log[Tan[x5 / x6]]^2), x4 -> -((3 x3 x4^2 y4 Sin[x1 x2] Sin[x3 x4^3]) / Log[Tan[x5 / x6]]^2), x5 -> -((2 y5 Cos[x3 x4^3] Csc[x5 / x6] Sec[x5 / x6] Sin[x1 x2]) / (x6 Log[Tan[x5 / x6]]^3)), x6 -> (2 x5 y6 Cos[x3 x4^3] Csc[x5 / x6] Sec[x5 / x6] Sin[x1 x2]) / (x6^2 Log[Tan[x5 / x6]]^3), x1 -> (x2 y1 Cos[x1 x2] Cos[x3 x4^3]) / Log[Tan[x5 / x6]]^2, x2 -> (x1 y2 Cos[x1 x2] Cos[x3 x4^3]) / Log[Tan[x5 / x6]]^2|>]
    ,
    TestID -> "Test-20161118-Y1D5O4"
];

Test[
    fd[x1, <|x1 -> y1|>]^fd[x2, <|x2 -> y2|>]
    ,
    fd[x1^x2, <|x1 -> x1^(-1 + x2) x2 y1, x2 -> x1^x2 y2 Log[x1]|>]
    ,
    TestID -> "Test-20161118-E6B3R4"
];

Test[
    Sin[fd[x + I y, <|x1 -> y1, x2 -> y2|>]]
    ,
    fd[Sin[x + I y], <|x1 -> y1 Cos[x + I y], x2 -> y2 Cos[x + I y]|>]
    ,
    TestID -> "Test-20161118-F4V1V7"
];

Test[
    {fd[1.23, <|x1 -> y1|>] < fd[4.56, <|x2 -> y2|>],
    fd[1.23, <|x1 -> y1|>] <= fd[4.56, <|x2 -> y2|>],
    fd[1.23, <|x1 -> y1|>] > fd[4.56, <|x2 -> y2|>],
    fd[1.23, <|x1 -> y1|>] >= fd[4.56, <|x2 -> y2|>]}
    ,
    {True, True, False, False}
    ,
    TestID -> "Test-20161215-U5J0S6"
];
  
  Test[
      fd[a, <|sa -> ya|>]/Sqrt[2]
      ,
      fd[a/Sqrt[2], <|sa -> ya/Sqrt[2]|>]
      ,
      TestID->"AD-20171002-J1W7F3"
  ];
    
Test[
    Inverse[{{fd[m11, <|t11 -> u11|>], m12}, {fd[m21, <|t21 -> u21|>], 
    m22}}]
    ,
    {{fd[m22/(-m12 m21 + 
    m11 m22), <|t21 -> (m12 m22 u21)/(-m12 m21 + m11 m22)^2, 
    t11 -> -((m22^2 u11)/(-m12 m21 + m11 m22)^2)|>], 
    fd[-(m12/(-m12 m21 + 
    m11 m22)), <|t21 -> -((m12^2 u21)/(-m12 m21 + m11 m22)^2), 
    t11 -> (m12 m22 u11)/(-m12 m21 + m11 m22)^2|>]}, {fd[-(
    m21/(-m12 m21 + 
    m11 m22)), <|t21 -> -((m12 m21 u21)/(-m12 m21 + m11 m22)^2) - 
    u21/(-m12 m21 + m11 m22), 
    t11 -> (m21 m22 u11)/(-m12 m21 + m11 m22)^2|>], 
    fd[m11/(-m12 m21 + 
    m11 m22), <|t11 -> -((m11 m22 u11)/(-m12 m21 + m11 m22)^2) + 
    u11/(-m12 m21 + m11 m22), 
    t21 -> (m11 m12 u21)/(-m12 m21 + m11 m22)^2|>]}}
    ,
    TestID->"AD-20171002-W1A2C4"
]
    
    Test[
        fd[x1, <|x1 -> y1|>]^fd[x2, <|x2 -> y2|>]
        ,
        fd[x1^x2, <|x1 -> x1^(-1 + x2) x2 y1, x2 -> x1^x2 y2 Log[x1]|>]
        ,
        TestID->"TestAD-20171003-D7P5T6"
    ]
    
    Test[
        Merge[{propagatefd[f, expr, <|x1 -> y1, t1 -> u1|>], 
        propagatefd[f, expr2, <|x1 -> y1, t2 -> u2|>]},  Total]
        ,
        <|x1 -> y1 Derivative[1][f][expr] + y1 Derivative[1][f][expr2], 
        t1 -> u1 Derivative[1][f][expr], t2 -> u2 Derivative[1][f][expr2]|>
        ,
        TestID->"TestAD-20171003-Z4N4L5"
    ]

Test[
    toValue[1.23]
    ,
    toValue[1.23]
    ,
    TestID->"TestAD-20171003-O4H5G9"
]

Test[
    toValue[fd[3 + x1, <|x1 -> y1|>]]
    ,
    3 + x1
    ,
    TestID->"TestAD-20171003-M1U2C7"
]
    
    Test[
        Power[fd[e1, <|x1 -> y1|>], 2]
        ,
        fd[e1^2, <|x1 -> 2 e1 y1|>]
        ,
        TestID->"TestAD-20171003-E7F8G3"
    ]
    
    Test[
        fd[e1, <|x1 -> y1|>] + fd[e2, <|t1 -> u1|>] + fd[e3, <|x1 -> y1|>] + 
        fd[e4, <|t1 -> u1|>]^2 + fd[e5, <|t1 -> u1|>] + 
        fd[e6, <|x1 -> y1|>] + 1/fd[e7, <|t1 -> u1|>]
        ,
        fd[e1 + e2 + e3 + e4^2 + e5 + e6 + 1/e7, <|x1 -> 3 y1, 
        t1 -> 2 u1 + 2 e4 u1 - u1/e7^2|>]
        ,
        TestID->"TestAD-20171003-A5F0H6"
    ]
    
    Test[
        fd[expr, <|x1 -> y1|>] >= Pi
        ,
        expr >= \[Pi]
        ,
        TestID->"TestAD-20171003-F1I8K0"
    ]
    
    Test[
        E <= fd[expr, <|x1 -> y1|>]
        ,
        E <= expr
        ,
        TestID->"TestAD-20171003-S0E0H2"
    ]
    
    Test[
        fd[expr, <|x1 -> y1|>] < fd[expr2, <|t1 -> u1|>]
        ,
        expr < expr2
        ,
        TestID->"TestAD-20171003-B4O4W4"
    ]
    
    Test[
        Max[fd[expr, <|x1 -> y1|>], 0]
        ,
        If[ 0 >= expr,
            0,
            fd[expr, <|x1 -> y1|>]
        ]
        ,
        TestID->"TestAD-20171003-M0E6B2"
    ]
    
    Test[
        fd[a, <|sa -> ya|>]/Sqrt[2]
        ,
        fd[a/Sqrt[2], <|sa -> ya/Sqrt[2]|>]
        ,
        TestID->"TestAD-20171003-Q2Z8X9"
    ]
    
    
	
	
	