let tests =
  [
    Test_Statics_Functions.tests,
    Test_Statics_FunctionSugar.tests,
    Test_Statics_Tuples.tests,
    Test_Statics_Lists.tests,
    Test_Statics_Polymorphism.tests,
    Test_Statics_Sums.tests,
    Test_Statics_ParameterizedTypes.tests,
    Test_Statics_Types.tests,
    Test_Statics_Modules.tests,
  ]
  @ Test_Statics_BuiltinsTupleOperations.tests
  @ [
    Test_Statics_Fixpoint.tests,
    Test_Statics_Properties.tests,
    Test_Statics_Parens.tests,
    Test_Statics_Slicing_Synthesis.tests,
    Test_Statics_Slicing_Analysis.tests,
    Test_Statics_Slicing_Binding.tests,
    Test_Statics_Slicing_InvalidQuery.tests,
    Test_Statics_Slicing_Properties.tests,
  ];
