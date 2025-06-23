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
      Polymorphism.out,
      Cards.out,
      Probes.out,
      Livelits.out,
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
      TESTSCasting.out,
      TESTSTypesandStaticErrors.out,
    ],
  ),
};
