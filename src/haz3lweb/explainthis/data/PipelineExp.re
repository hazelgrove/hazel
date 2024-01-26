open Haz3lcore;
open ExplainThisForm;
open Example;

/* (A) Use this file as an example for adding a new form to ExplainThis.
 * You should be able to copy-paste this file and modify it to add a new form */

let single = (~arg_id: Id.t, ~fn_id: Id.t): Simple.t => {
  /* (B) You'll need to add new cases to ExplainThisForm.re for the new form
   * to represent a group_id and form_id. This Simple style is specialized
   * to singleton groups. In general, the group_id needs to be unique, and
   * form_ids need to be unique within a group. These ids are used to track
   * ExplainThis persistent state. */
  group_id: PipelineExp,
  form_id: PipelineExp,
  /* (C) The abstract field defines an abstract example illustrating the
   * new form. You'll need to provide pairs associating any representative
   * subterms of the exemplar (e.g. "e_arg" and "e_fun" below) with the
   * concrete subterms of the term the user has selected (here, arg_id
   * and fn_id). You'll then need a function to construct a segment
   * representing your abstract. This is done in this indirect way so
   * as to associate representative and concrete subterms ids for
   * syntax highlighting purposes. */
  abstract:
    Simple.mk_2(("e_arg", arg_id), ("e_fun", fn_id), (e_arg', e_fn') =>
      [e_arg', space(), pipeline(), space(), e_fn']
    ),
  /* (D) The explanation which will appear in the sidebar below the abstract */
  explanation:
    Printf.sprintf(
      "Passes the [*argument*](%s) to the [*function*](%s).",
      arg_id |> Id.to_string,
      fn_id |> Id.to_string,
    ),
  /* (E) Additional more concrete examples and associated explanations */
  examples: [
    {
      sub_id: Pipeline1,
      term: mk_example("1 |> fun x -> x + 1"),
      message: {|
              The argument 1 is passed to an increment function, and the entire expression evaluates to 2.
              The pipeline operator is useful for chaining functions together.
              |},
    },
  ],
};
