open Haz3lcore;

let startup: PersistentData.t = {
  scratch: (
    0,
    [
      (
        "Scratchpad 1",
        {
          editor: Zipper.init() |> PersistentZipper.persist,
          result: EvalResult.Model.init |> EvalResult.Model.persist,
        },
      ),
    ],
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
      B2t2.Datasheet.slide,
      B2T2ExampleTables.out,
      B2T2TableAPIConstructors.out,
      B2T2TableAPIProperties.out,
      B2T2TableAPIAccessSubcomponents.out,
      B2T2TableAPISubtable.out,
      B2T2TableAPISubtablePart2.out,
      B2T2TableAPIOrdering.out,
      B2T2TableAPIAggregate.out,
      B2T2TableAPIMissingValues.out,
      B2T2TableAPIDataCleaning.out,
      B2T2TableAPIUtilitiesPart1.out,
      B2T2TableAPIUtilities2.out,
      B2T2ExampleProgramDotProduct.out,
      B2T2ExampleProgrampHackingHomogeneous.out,
      B2T2ExampleProgrampHackingHeterogeneous.out,
      B2T2ExampleProgramquizScoreFilter.out,
      B2T2ExampleProgramquizScoreSelect.out,
      B2T2ExampleProgramgroupByRetentive.out,
      B2T2ExampleProgramsgroupBySubtractive.out,
      B2T2ErrorsMalformedTables.out,
      B2T2ErrorsUsingTables.out,
      B2T2ErrorsUsingTables2.out,
      B2T2ErrorsUsingTables3.out,
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
    ]
    |> List.map(((name, content)) =>
         (
           name,
           {
             editor: content,
             result: EvalResult.Model.init |> EvalResult.Model.persist,
           }: CellEditor.Model.persistent,
         )
       ),
  ),
};
