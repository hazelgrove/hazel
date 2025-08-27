open Haz3lcore;
open Util;

let empty_cell_editor_persistent: unit => CellEditor.Model.persistent =
  () => {
    editor: Zipper.init() |> PersistentZipper.persist,
    result: EvalResult.Model.init |> EvalResult.Model.persist,
  };

let startup: PersistentData.t = {
  scratch: (0, [("Scratchpad 1", empty_cell_editor_persistent())]),
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
      B2T2TableAPIConstructorsemptyTable.out,
      B2T2TableAPIConstructorsaddRows.out,
      B2T2TableAPIConstructorsaddColumn.out,
      B2T2TableAPIConstructorsbuildColumn.out,
      B2T2TableAPIConstructorsvcat.out,
      B2T2TableAPIConstructorshcat.out,
      B2T2TableAPIConstructorsvalues.out,
      B2T2TableAPIConstructorscrossJoin.out,
      B2T2TableAPIConstructorsleftJoin.out,
      B2T2TableAPIProperties.out,
      B2T2TableAPIAccessSubcomponents.out,
      B2T2TableAPISubtable.out,
      B2T2TableAPIOrdering.out,
      B2T2TableAPIAggregate.out,
      B2T2TableAPIMissingValues.out,
      B2T2TableAPIDataCleaning.out,
      B2T2TableAPIUtilitiesFlatten.out,
      B2T2TableAPIUtilitiestransformColumn.out,
      B2T2TableAPIUtilitiesrenameColumns.out,
      B2T2TableAPIUtilitiesfind.out,
      B2T2TableAPIUtilitiesgroupByRetentive.out,
      B2T2TableAPIUtilitiesgroupBySubtractive.out,
      B2T2TableAPIUtilitiesupdate.out,
      B2T2TableAPIUtilitiesselect.out,
      B2T2TableAPIUtilitiesselectMany.out,
      B2T2TableAPIUtilitiesgroupJoin.out,
      B2T2TableAPIUtilitiesjoin.out,
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

let find_documentation_slide = (name: string) => {
  startup.documentation
  |> snd
  |> List.find_opt(((n, _)) => n == name)
  |> Option.map(snd);
};

let default_documentation_slide_name =
    (name: string): CellEditor.Model.persistent => {
  OptUtil.get(
    () => empty_cell_editor_persistent(),
    find_documentation_slide(name),
  );
};
