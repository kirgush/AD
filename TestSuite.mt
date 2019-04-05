(* Wolfram Language Test file *)

dir = "Test";
files = {"TestAD.mt", "TestSFunctions.mt", "TestCDF"};
TestSuite[FileNameJoin[{dir, #}] & /@ files];