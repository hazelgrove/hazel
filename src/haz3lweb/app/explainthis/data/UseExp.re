open Haz3lcore;
open ExplainThisForm;
open Example;

let single = (~typ_id: Id.t, ~body_id: Id.t): Simple.t => {
  group_id: UseExp,
  form_id: UseExp,
  abstract:
    Simple.mk_2(("e_typ", typ_id), ("e_body", body_id), (e_typ', e_body') =>
      [mk_use([[space(), e_typ', space()]]), space(), e_body']
    ),
  explanation:
    Printf.sprintf(
      "Within the [*body*](%s), number literals and operations use type [*type*](%s) by default. The type provided must be one of Int, SInt, Nat, or Float.",
      body_id |> Id.to_string,
      typ_id |> Id.to_string,
    ),
  examples: [
    {
      sub_id: UseExp1,
      term: mk_example("use SInt in 1 + 2"),
      message: {|
                The expression 1 + 2 is of type SInt (System Int).
                The use keyword is used to specify the type of number literals and operations.
                |},
    },
  ],
};
