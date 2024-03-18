open Haz3lcore;
open ExplainThisForm;
open Example;

let single = (~mod_id: Id.t, ~mem_id: Id.t): Simple.t => {
  group_id: DotTyp,
  form_id: DotTyp,
  abstract:
    Simple.mk_2(("ModuleVar", mod_id), ("t", mem_id), (t_mod', t_mem') =>
      [t_mod', space(), dot_typ(), space(), t_mem']
    ),
  explanation:
    Printf.sprintf(
      "Dot access. Access the [*type member*](%s) of the [*module*](%s).",
      mem_id |> Id.to_string,
      mod_id |> Id.to_string,
    ),
  examples: [
    {
      sub_id: DotTyp,
      term:
        mk_example("module M =\ntype T = Int in\nin\nlet x : M.T = 1 in\nx"),
      message: {|
      The module M has a type member T which is alias of Int, so type M.T is consistent with type Int.
              |},
    },
  ],
};
