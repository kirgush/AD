(* Wolfram Language Test file *)

Test[stheta[0, 0], 0
  ,
  TestID -> "TestSFunctions-20170123-N8G8R2"];
Test[stheta[0., 0], 0
  ,
  TestID -> "TestSFunctions-20170123-A7J4D2"];
Test[stheta[{{0, 0, 0, 0.`}, {0, 0, 0, 0}, {0, 0.`, 0, 0}}, 0], {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}
  ,
  TestID -> "TestSFunctions-20170123-I9Q4P0"];
Test[stheta[{{fd[0, <|x -> 1.23`|>], fd[0, <|x -> 1.23`|>], fd[0.`, <|y -> -0.23`|>]},
  {fd[0, <|x -> 1.23`|>], fd[0, <|x -> 1.23`|>], fd[0.`, <|y -> -0.23`|>]}}, 0], {{0, 0, 0}, {0, 0, 0}}
  ,
  TestID -> "TestSFunctions-20170123-V2L5W3"];