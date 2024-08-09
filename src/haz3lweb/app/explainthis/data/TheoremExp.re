// Note(Thomas): This should be analogous to the let case, but should not
// display as a let in ExplainThis. This involves a refactor to avoid duplciation.
// open Haz3lcore;
// open Example;
// open ExplainThisForm;
// let _pat = pat("var");
// let _e_def = exp("e_def");
// let _e_body = exp("e_body");
// let theorem_typ_coloring_ids =
//     (~pat_id: Id.t, ~e_def_id: Id.t, ~e_body_id: Id.t): list((Id.t, Id.t)) => [
//   (Piece.id(_pat), pat_id),
//   (Piece.id(_e_def), e_def_id),
//   (Piece.id(_e_body), e_body_id),
// ];
// let single = (~pat_id: Id.t, ~e_def_id: Id.t, ~e_body_id: Id.t): Simple.t => {
//   group_id: LetExp,
//   form_id: LetExp,
//   abstract:
//     Simple.mk_2(
//       ("pat", pat_id),
//       ("e_def", e_def_id),
//       ("e_body", e_body_id),
//       (pat', e_def', e_body') =>
//       [mk_theorem([[pat'], [e_def'], [e_body']])]
//     ),
//   explanation:
//     Printf.sprintf(
//       "The [*definition*](%s) is bound to the [*variable*](%s) `%s` in the [*body*](%s).",
//       pat_id |> Id.to_string,
//       e_def_id |> Id.to_string,
//       e_body_id |> Id.to_string,
//     ),
//   examples: [],
// };
