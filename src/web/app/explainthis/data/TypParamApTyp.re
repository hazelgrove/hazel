open Haz3lcore;
open Example;
open ExplainThisForm;

let option_ex = {
  sub_id: TypParamAp1,
  term: mk_example("let x : Option(Int) = Some(5) in x"),
  message: "Option is a parameterized type; Option(Int) is the type + None + Some(Int).",
};

let either_ex = {
  sub_id: TypParamAp2,
  term: mk_example("let x : Either(Int, Bool) = R(true) in x"),
  message: "Either takes two type arguments at once; Either(Int, Bool) is the type + L(Int) + R(Bool).",
};

/* `T(X)`, `T(X1, X2)`: applying a type function (typically a
   parameterized alias) to type arguments. */
let typ_param_ap = (~callee_id: Id.t, ~arg_ids: list(Id.t)): form => {
  let n = List.length(arg_ids);
  let callee = typ("T");
  let (args_seg, arg_pieces) = typ_list("X", n);
  {
    id: TypParamApTyp,
    syntactic_form: [callee, mk_ap_typ([args_seg])],
    colorings:
      [(Piece.id(callee), callee_id)] @ zip_colorings(arg_pieces, arg_ids),
    expandable_id: None,
    explanation:
      Printf.sprintf(
        "Applies the [*type function*](%s) to the type %s %s, giving the type its definition describes with %s for its parameter%s.",
        Id.to_string(callee_id),
        n == 1 ? "argument" : "arguments",
        links("argument", arg_ids),
        n == 1 ? "the argument substituted" : "the arguments substituted",
        n == 1 ? "" : "s",
      ),
    examples: n == 1 ? [option_ex] : [either_ex],
  };
};

let typ_param_aps = (~callee_id: Id.t, ~arg_ids: list(Id.t)): group =>
  singleton(typ_param_ap(~callee_id, ~arg_ids));

/* The comma-separated argument list `X1, X2` of `T(X1, X2)`. */
let typ_args = (~arg_ids: list(Id.t)): form => {
  let n = List.length(arg_ids);
  let (args_seg, arg_pieces) = typ_list("X", n);
  {
    id: TypArgsTyp,
    syntactic_form: args_seg,
    colorings: zip_colorings(arg_pieces, arg_ids),
    expandable_id: None,
    explanation:
      Printf.sprintf(
        "The %d type arguments %s of a type application, supplied together: the type function applied must take exactly %d.",
        n,
        links("argument", arg_ids),
        n,
      ),
    examples: [either_ex],
  };
};

let typ_argss = (~arg_ids: list(Id.t)): group =>
  singleton(typ_args(~arg_ids));
