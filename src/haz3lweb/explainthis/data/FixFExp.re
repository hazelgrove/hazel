open Haz3lcore;
open ExplainThisForm;
open Example;

let single = (~pat_id: Id.t, ~body_id: Id.t): Simple.t => {
  /* (B) You'll need to add new cases to ExplainThisForm.re for the new form
   * to represent a group_id and form_id. This Simple style is specialized
   * to singleton groups. In general, the group_id needs to be unique, and
   * form_ids need to be unique within a group. These ids are used to track
   * ExplainThis persistent state. */
  group_id: FixExp(Base),
  form_id: FixExp(Base),
  /* (C) The abstract field defines an abstract example illustrating the
   * new form. You'll need to provide pairs associating any representative
   * subterms of the exemplar (e.g. "e_arg" and "e_fun" below) with the
   * concrete subterms of the term the user has selected (here, arg_id
   * and fn_id). You'll then need a function to construct a segment
   * representing your abstract. This is done in this indirect way so
   * as to associate representative and concrete subterms ids for
   * syntax highlighting purposes. */
  abstract:
    Simple.mk_2(("p", pat_id), ("e", body_id), (p, e) =>
      [mk_fix([[space(), p, space()]]), space(), e]
    ),
  /* (D) The explanation which will appear in the sidebar below the abstract */
  explanation:
    Printf.sprintf(
      "Recursively replaces all occurences of the [*pattern*](%s) inside the [*body*](%s) with the entire [*body*](%s) itself, effectively creating an infinite expression. Unless [*pattern*](%s) is a function, it is likely to evaluate forever.",
      pat_id |> Id.to_string,
      body_id |> Id.to_string,
      body_id |> Id.to_string,
      pat_id |> Id.to_string,
    ),
  /* (E) Additional more concrete examples and associated explanations */
  examples: [
    {
      sub_id: Fix1,
      term: mk_example("fix x -> x + 1"),
      message: {|
              Tries to create the infinite expression (((...) + 1) + 1) + 1 but times out
              |},
    },
    {
      sub_id: Fix2,
      term:
        mk_example(
          "(fix f -> fun x -> \nif x == 0 then \n0 \nelse \nf(x-1) + 2\n) (5)",
        ),
      message: {|
             A recursive function that doubles a given number.
              |},
    },
  ],
};
