open Haz3lcore;
open Example;
open ExplainThisForm;

let _pat = pat("var");
let _typ_arg = typ("ty_arg");
let forall_typ_coloring_ids =
    (~pat_id: Id.t, ~tbody_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_pat), pat_id),
  (Piece.id(_typ_arg), tbody_id),
];

// let forall_typ: form = {
//   let explanation = "This forall type states that for every value [*type variable*](%s), the type [*instantiated type*](%s) is inhabited.";
//   {
//     id: ForallTyp,
//     syntactic_form: [mk_type([[space(), _pat, space()]]), _typ_arg],
//     expandable_id: Some((Piece.id(_pat), [_typ_arg])),
//     explanation,
//     examples: [],
//   };
// };

// let forall_typ: group = {id: ForallTyp, forms: [forall_typ]};

/* (A) Use this file as an example for adding a new form to ExplainThis.
 * You should be able to copy-paste this file and modify it to add a new form */

let single = (~pat_id: Id.t, ~ty_arg_id: Id.t): Simple.t => {
  /* (B) You'll need to add new cases to ExplainThisForm.re for the new form
   * to represent a group_id and form_id. This Simple style is specialized
   * to singleton groups. In general, the group_id needs to be unique, and
   * form_ids need to be unique within a group. These ids are used to track
   * ExplainThis persistent state. */
  group_id: ForallTyp,
  form_id: ForallTyp,
  /* (C) The abstract field defines an abstract example illustrating the
   * new form. You'll need to provide pairs associating any representative
   * subterms of the exemplar (e.g. "e_arg" and "e_fun" below) with the
   * concrete subterms of the term the user has selected (here, arg_id
   * and fn_id). You'll then need a function to construct a segment
   * representing your abstract. This is done in this indirect way so
   * as to associate representative and concrete subterms ids for
   * syntax highlighting purposes. */
  abstract:
    Simple.mk_2(("x", pat_id), ("t_body", ty_arg_id), (pat', typ_arg') =>
      [mk_forall([[space(), pat', space()]]), space(), typ_arg']
    ),
  /* (D) The explanation which will appear in the sidebar below the abstract */
  explanation:
    Printf.sprintf(
      "This forall type states that for every value [*x*](%s), the [*body*](%s) type is inhabited.",
      pat_id |> Id.to_string,
      ty_arg_id |> Id.to_string,
    ),
  /* (E) Additional more concrete examples and associated explanations */
  examples: [] // {
  //   sub_id: Pipeline1,
  //   term: mk_example("1 |> fun x -> x + 1"),
  //   message: {|
  //           The argument 1 is passed to an increment function, and the entire expression evaluates to 2.
  //           The pipeline operator is useful for chaining functions together.
  //           |},
  // },
};
