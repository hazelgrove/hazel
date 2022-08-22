open Tezt;

/* include TeztExamples; */

include Test_Hazeltext;

/* hazelcore */

include Test_HTyp;
include Test_SkelParser;
include Test_TyVar;

let () = Test.run();
