open Haz3lcore;

let startup: PersistentData.t = {
  scratch: (
    0,
    [("Scratchpad 1", Zipper.init() |> PersistentZipper.persist)],
  ),
  documentation: (
    0,
    [
      BasicReference.out,
      Projectors.out,
      ADTs.out,
      Tuples.out,
      Tables.out,
      Polymorphism.out,
      Cards.out,
      Probes.out,
      Livelits.out,
      BuiltinDataframeOperationDemo.out,
      B2T2Datasheet.out,
      B2T2ExampleTables.out,
      B2T2TableAPI.out,
      B2T2TableAPI2.out,
      B2T2TableAPIUtilities.out,
      GUIDEExpressiveProgramming.out,
      GUIDEComposingExpressions.out,
      GUIDEComputingEquationally.out,
      GUIDEVariables.out,
      GUIDECompositionality.out,
      GUIDEScope.out,
      GUIDEShadowing.out,
      GUIDEBoolsandTypes.out,
      GUIDEConditionals.out,
      GUIDEFunctions.out,
      TESTSTypesandStaticErrors.out,
    ],
  ),
};
