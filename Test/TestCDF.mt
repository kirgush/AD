(* Wolfram Language Test file *)

$testTolerance = 10^(-6);
$sameTest = Abs[#1 -#2] < $testTolerance &;
    
Test[
    Module[ {mean, covMat, x},
        mean = {0, 0, 0};
        covMat = {{0.5 0.5 , -0.9 0.5  0.3, -0.6 0.5 0.6}, {-0.9 0.5  0.3, 
        0.3 0.3, 0.75 0.3 0.6}, {-0.6 0.5 0.6, 0.75 0.3 0.6, 
        0.6 0.6}};
        x =  {-0.04, 0.1, 0.06};
        Quiet@bigPhi[mean, covMat, x] / CDF[MultinormalDistribution[mean, covMat], x]
    ]
    ,
    1.0
    ,
    SameTest->$sameTest
    ,
    TestID->"TestCDF-20180611-J5S5R2"
]

Test[
    Module[ {mean, covMat, x},
        mean = {0, 0, 0};
        covMat = {{0.5 0.5 , -0.95 0.5  0.3, -0.6 0.5 0.6}, {-0.95 0.5  0.3, 
        0.3 0.3, 0.75 0.3 0.6}, {-0.6 0.5 0.6, 0.75 0.3 0.6, 
        0.6 0.6}};
        x =  {-0.04, 0.1, 0.06};
        Quiet@bigPhi[mean, covMat, x] / CDF[MultinormalDistribution[mean, covMat], x]
    ]
    ,
    1.0
    ,
    SameTest->$sameTest
	,
	TestID->"TestCDF-20180611-L3J3H7"
   
]

Test[
    Module[ {mean, vol1, vol2, vol3, rho12, rho13, rho23, covMat, x},
        mean = {0, 0, 0};
        vol1 = 0.5;
        vol2 = 0.3;
        vol3 = 0.7;
        rho12 = -0.999;
        rho13 = -0.6;
        rho23 = 0.6;
        covMat = {{vol1 vol1, rho12 vol1 vol2, 
           rho13 vol1 vol3}, {rho12 vol1 vol2, vol2 vol2, 
           rho23 vol2 vol3}, {rho13 vol1 vol3, rho23 vol2 vol3, vol3 vol3}};
        x = {-0.04, 0.1, 0.06};
        Quiet@bigPhi[mean, covMat, x] / CDF[MultinormalDistribution[mean, covMat], x]
    ]
    ,
    1.0
    ,
    SameTest->$sameTest
    ,
    TestID->"TestCDF-20180611-K0Q3L5"
]