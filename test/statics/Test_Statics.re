let tests =
  [
    Test_Statics_Functions.tests,
    Test_Statics_Tuples.tests,
    Test_Statics_Lists.tests,
    Test_Statics_Polymorphism.tests,
    Test_Statics_Sums.tests,
    Test_Statics_Types.tests,
  ]
  @ Test_Statics_BuiltinsTupleOperations.tests
  @ [Test_Statics_Fixpoint.tests, Test_Statics_Properties.tests];
