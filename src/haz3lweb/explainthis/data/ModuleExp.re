open Haz3lcore;
open ExplainThisForm;
open Example;

let single = (~pat_id: Id.t, ~def_id: Id.t, ~body_id: Id.t): Simple.t => {
  group_id: ModuleExp,
  form_id: ModuleExp,
  abstract:
    Simple.mk_3(
      ("M", pat_id),
      ("e_def", def_id),
      ("e_body", body_id),
      (pat', e_def', e_body') =>
      [
        mk_module([[space(), pat', space()], [space(), e_def', space()]]),
        linebreak(),
        e_body',
      ]
    ),
  explanation:
    Printf.sprintf(
      "Module definition expression. The variables defined in [*definition*](%s) is packaged as a module and bound to the [*module variable*](%s) in the [*body*](%s).",
      def_id |> Id.to_string,
      pat_id |> Id.to_string,
      body_id |> Id.to_string,
    ),
  examples: [
    {
      sub_id: Module,
      term: mk_example("module M =\nlet x = 1 in\nin\nM.x"),
      message: {|
      A module with a member x=1 is created and assigned to tag M, so the expression is evaluated to 1.
              |},
    },
    {
      sub_id: ModuleWithType,
      term:
        mk_example(
          "module OptInt =\ntype OptInt = None + Some(Int) in\nin\nlet x:OptInt = Some(1) in\nx",
        ),
      message: {|
      The type member in a module with the same name as the module can be used directly in the body.
              |},
    },
  ],
};
