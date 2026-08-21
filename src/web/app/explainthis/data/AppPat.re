open Haz3lcore;
open Example;
open ExplainThisForm;

let pat_con = pat("p_con");
let pat_arg = pat("p_arg");
let conapp_pat_coloring_ids =
    (~x_id: Id.t, ~arg_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(pat_con), x_id),
  (Piece.id(pat_arg), arg_id),
];
let conapp_pat_form = [pat_con, mk_ap_pat([[pat_arg]])];
let conapp_pat = (~x_id: Id.t, ~arg_id: Id.t): form => {
  id: ApConsPat,
  syntactic_form: conapp_pat_form,
  colorings: conapp_pat_coloring_ids(~x_id, ~arg_id),
  expandable_id: None,
  explanation:
    Stdlib.Printf.sprintf(
      "Only expressions that match the [*constructor*](%s) with an *argument* matching the [*argument pattern*](%s) match this *constructor application pattern*.",
      Id.to_string(x_id),
      Id.to_string(arg_id),
    ),
  examples: [],
};

let pat_fun = pat("fun");
let pat_arg = pat("p_arg");
let funapp_pat_coloring_ids =
    (~x_id: Id.t, ~arg_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(pat_fun), x_id),
  (Piece.id(pat_arg), arg_id),
];
let funapp_pat_form = [pat_fun, mk_ap_pat([[pat_arg]])];
let funapp_pat = (~x_id: Id.t, ~arg_id: Id.t): form => {
  id: ApFuncPat,
  syntactic_form: funapp_pat_form,
  colorings: funapp_pat_coloring_ids(~x_id, ~arg_id),
  expandable_id: None,
  explanation:
    Stdlib.Printf.sprintf(
      "Defines a function [*function*](%s) with [*arguments*](%s).",
      Id.to_string(x_id),
      Id.to_string(arg_id),
    ),
  examples: [],
};

/* Curried function-definition sugar `f(x)(y)…`: one argument pattern per
   group, each linked, rather than reading `f(x)` as the function. */
let curried_args_phrase = (arg_ids: list(Id.t)): string =>
  arg_ids
  |> List.mapi(~f=(i, id) =>
       Printf.sprintf(
         i == 0
           ? "with [*arguments*](%s)"
           : ", returning a function with [*arguments*](%s)",
         Id.to_string(id),
       )
     )
  |> String.concat(~sep="");

let curried_funapp_pat = (~x_id: Id.t, ~arg_ids: list(Id.t)): form => {
  let args =
    List.mapi(arg_ids, ~f=(i, _) => pat("p_arg" ++ string_of_int(i + 1)));
  {
    id: ApFuncPat,
    syntactic_form: [pat_fun, ...List.map(args, ~f=a => mk_ap_pat([[a]]))],
    colorings: [
      (Piece.id(pat_fun), x_id),
      ...List.map2_exn(args, arg_ids, ~f=(a, id) => (Piece.id(a), id)),
    ],
    expandable_id: None,
    explanation:
      Printf.sprintf(
        "Defines a curried [*function*](%s) %s.",
        Id.to_string(x_id),
        curried_args_phrase(arg_ids),
      ),
    examples: [],
  };
};

let curried_funaps = (~x_id: Id.t, ~arg_ids: list(Id.t)): group =>
  singleton(curried_funapp_pat(~x_id, ~arg_ids));

let conaps = (~x_id: Id.t, ~arg_id: Id.t): group =>
  singleton(conapp_pat(~x_id, ~arg_id));

let funaps = (~x_id: Id.t, ~arg_id: Id.t): group =>
  singleton(funapp_pat(~x_id, ~arg_id));
