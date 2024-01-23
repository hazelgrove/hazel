open Haz3lcore;
open ExplainThisForm;
open Example;

/* (A) Use this file as an example for adding a new form to ExplainThis.
 * You should be able to copy-paste this file and modify it to add a new form */

/* (B) Specify subterms of the syntactic form here. These must be specified
 * externally so their UUIDs can be used to form an association between the
 * subterms of the displayed form (the 'syntatic_form' field below) and the
 * color highlights on the program syntax (controlled by field 'colorings') */
let (pipe_arg, pipe_fun) = (exp("e_arg"), exp("e_fun"));

let single = (~arg_id: Id.t, ~fn_id: Id.t): single_doc => {
  /* (C) You'll need to add new cases to ExplainThisForm.re for the new form
   * to represent the group_id and form_id. The group_id needs to be unique,
   * and form_ids need to be unique within a group. These ids are used to
   * track ExplainThis persistent state. */
  group_id: PipelineExp,
  form_id: PipelineExp,
  /* (D) The next field should be a segment representing an exemplar of the
   * new form. Add any necessary helpers to construct the required pieces above,
   * or add them to Example.re if they're more general. Make sure that any
   * sub-terms used are defined in (B) so they can be reused in (E) below */
  syntactic_form: [pipe_arg, space(), pipeline(), space(), pipe_fun],
  /* (E) This field should be constructable more or less mechanically; see (B) */
  colorings: [(Piece.id(pipe_arg), arg_id), (Piece.id(pipe_fun), fn_id)],
  explanation:
    Printf.sprintf(
      "Passes the [*argument*](%s) to the [*function*](%s).",
      arg_id |> Id.to_string,
      fn_id |> Id.to_string,
    ),
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
