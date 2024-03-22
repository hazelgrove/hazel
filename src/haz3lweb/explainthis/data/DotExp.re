open Haz3lcore;
open ExplainThisForm;
open Example;

let single = (~mod_id: Id.t, ~mem_id: Id.t): Simple.t => {
  group_id: DotExp,
  form_id: DotExp,
  abstract:
    Simple.mk_2(("Module_name", mod_id), ("x", mem_id), (e_mod', e_mem') =>
      [e_mod', space(), dot_exp(), space(), e_mem']
    ),
  explanation:
    Printf.sprintf(
      "Dot access. Retrieves the value of the [*member*](%s) x of the [*module*](%s).",
      mem_id |> Id.to_string,
      mod_id |> Id.to_string,
    ),
  examples: [
    {
      sub_id: Dot,
      term: mk_example("module M = \nlet x = 1 in\nin\nM.x"),
      message: {|
      The module M has a member x which is bound to 1, so the expression evaluates to 1.
              |},
    },
  ],
};
