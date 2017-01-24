(* Wolfram Language Test file *)

dir = "Test";
files = {"TestAD.mt", "TestSFunctions.mt"};
TestSuite[FileNameJoin[{dir, #}] & /@ files];