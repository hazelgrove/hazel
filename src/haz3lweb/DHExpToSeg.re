open Haz3lcore;

let ws = _ => Base.Secondary({id: Id.mk(), content: Whitespace(Form.space)});
let lb = _ =>
  Base.Secondary({id: Id.mk(), content: Whitespace(Form.linebreak)});
let hole = _ => Base.Grout({id: Id.mk(), shape: Convex});

let pad_r = child => child @ [ws()];
let pad_l = child => [ws()] @ child;
let pad = child => [ws()] @ child @ [ws()];

let atomic_operand = (sort: Sort.t, s: string): Piece.t => {
  let label = [s];
  let mold =
    switch (Molds.get(label) |> List.filter((m: Mold.t) => m.out == sort)) {
    | [] => Mold.mk_op(Any, [])
    | [hd, ..._] => hd
    };
  Piece.Tile({id: Id.mk(), label, mold, shards: [0], children: []});
};

let atomic_operator = (s: string, p: Precedence.t, sort: Sort.t) => {
  let label = [s];
  let mold = Mold.mk_bin(p, sort, []);
  Piece.Tile({id: Id.mk(), label, mold, shards: [0], children: []});
};

let commas = (sort: Sort.t, n): list(Segment.t) =>
  List.init(n, _ => [atomic_operator(",", Precedence.prod, sort), ws()]);

let tuple = (sort: Sort.t, go: 'a => Segment.t, ds: list('a)): Segment.t =>
  ds == []
    ? []
    : Util.ListUtil.interleave(
        List.map(go, ds),
        List.init(List.length(ds) - 1, _ =>
          [atomic_operator(",", Precedence.prod, sort), ws()]
        ),
      )
      |> List.flatten;

let parens_tile = (sort: Sort.t, child: Segment.t): Piece.t =>
  Tile({
    id: Id.mk(),
    label: ["(", ")"],
    mold: Mold.mk_op(sort, [sort]),
    shards: [0, 1],
    children: [child],
  });

let listlit_tile = (child: Segment.t): Piece.t =>
  Tile({
    id: Id.mk(),
    label: ["[", "]"],
    mold: Mold.mk_op(Exp, [Exp]),
    shards: [0, 1],
    children: [child],
  });

let ap_tile = (sort: Sort.t, child: Segment.t): Piece.t =>
  Tile({
    id: Id.mk(),
    label: ["(", ")"],
    mold: Mold.mk_post(Precedence.ap, sort, [sort]),
    shards: [0, 1],
    children: [child],
  });

let fun_tile = (pat: Segment.t): Piece.t =>
  Tile({
    id: Id.mk(),
    label: ["fun", "->"],
    mold: Mold.mk_pre(Precedence.fun_, Exp, [Pat]),
    shards: [0, 1],
    children: [pad(pat)],
  });

let let_tile = (pat: Segment.t, def: Segment.t): Piece.t =>
  Tile({
    id: Id.mk(),
    label: ["let", "=", "in"],
    mold: Mold.mk_pre(Precedence.let_, Exp, [Pat, Exp]),
    shards: [0, 1, 2],
    children: [pad(pat), pad(def)],
  });

let test_tile = (child: Segment.t): Piece.t =>
  Tile({
    id: Id.mk(),
    label: ["test", "end"],
    mold: Mold.mk_op(Exp, [Exp]),
    shards: [0, 1],
    children: [pad(child)],
  });

let case_tile = (child: Segment.t): Piece.t =>
  Tile({
    id: Id.mk(),
    label: ["case", "end"],
    mold: Mold.mk_op(Exp, [Rul]),
    shards: [0, 1],
    children: [pad(child)],
  });

let rule_tile = (pat: Segment.t): Piece.t =>
  Tile({
    id: Id.mk(),
    label: ["|", "=>"],
    mold: Mold.mk_bin'(Precedence.rule_sep, Rul, Exp, [Pat], Exp),
    shards: [0, 1],
    children: [pad(pat)],
  });

let rec infix_exp = (d1, s, d2, p) =>
  go(d1) @ [atomic_operator(s, p, Exp)] @ go(d2)
and infix_exp_pad = (d1, s, d2, p) =>
  go(d1) @ [ws(), atomic_operator(s, p, Exp), ws()] @ go(d2)
and go = (d: DHExp.t): Segment.t => {
  let prec = DHDoc_Exp.precedence(~show_casts=false, d);
  switch (d) {
  // Ignore these wrappers
  | NonEmptyHole(_, _, _, d) => go(d)
  | Cast(d, _, _) => go(d)
  | FailedCast(d, _, _) => go(d)
  | InvalidOperation(d, _) => go(d)
  | Closure(_, d) => go(d)
  | FixF(_, _, d) => go(d)
  // Atomic operands
  | EmptyHole(_) => [hole()]
  | ExpandingKeyword(_, _, kw) => [
      atomic_operand(Exp, ExpandingKeyword.to_string(kw)),
    ]
  | BoolLit(b) => [atomic_operand(Exp, string_of_bool(b))]
  | IntLit(i) => [atomic_operand(Exp, string_of_int(i))]
  | FloatLit(f) => [atomic_operand(Exp, string_of_float(f))]
  | StringLit(s) => [atomic_operand(Exp, Form.string_quote(s))]
  | BoundVar(s)
  | Constructor(s)
  | FreeVar(_, _, s)
  | InvalidText(_, _, s) => [atomic_operand(Exp, s)]
  // Composite value forms
  | Ap(TestLit(_), d2) => [test_tile(go(d2))]
  | Ap(d1, d2) => go(d1) @ [ap_tile(Exp, go(d2))]
  | Tuple([]) => [atomic_operand(Exp, "()")]
  | Tuple(ds) =>
    //TODO: only parenthesize selectively?
    [parens_tile(Exp, tuple(Exp, go, ds))]
  | ListLit(_, _, _, []) => [atomic_operand(Exp, "[]")]
  | ListLit(_, _, _, ds) => [listlit_tile(tuple(Exp, go, ds))]
  // Composite infix operators
  // TODO(andrew): parenthesization
  | Cons(d1, d2) => infix_exp(d1, "::", d2, prec)
  | Sequence(d1, d2) => infix_exp(d1, ";", d2, prec)
  | ListConcat(d1, d2) => infix_exp_pad(d1, "@", d2, prec)
  | BinBoolOp(op, d1, d2) =>
    infix_exp_pad(d1, Term.UExp.bool_op_to_string(op), d2, prec)
  | BinIntOp(op, d1, d2) =>
    infix_exp_pad(d1, Term.UExp.int_op_to_string(op), d2, prec)
  | BinFloatOp(op, d1, d2) =>
    infix_exp_pad(d1, Term.UExp.float_op_to_string(op), d2, prec)
  | BinStringOp(op, d1, d2) =>
    infix_exp_pad(d1, Term.UExp.string_op_to_string(op), d2, prec)
  // Other Composites
  | ApBuiltin(s, []) =>
    [atomic_operand(Exp, s)]
    @ [ap_tile(Exp, [atomic_operand(Exp, "()")])]
  | ApBuiltin(s, ds) =>
    [atomic_operand(Exp, s)] @ [ap_tile(Exp, tuple(Exp, go, ds))]
  | Fun(p, _, d, _) => [fun_tile(go_pat(p))] @ pad_l(go(d))
  | Let(p, d1, d2) => [let_tile(go_pat(p), go(d1))] @ [lb()] @ go(d2)
  | ConsistentCase(Case(d, rs, _))
  | InconsistentBranches(_, _, Case(d, rs, _)) =>
    let child =
      List.map(
        (DHExp.Rule(p, d)) =>
          pad_l([rule_tile(go_pat(p))]) @ pad_l(go(d)) @ [lb()],
        rs,
      )
      |> List.flatten;
    [case_tile(go(d) @ [lb()] @ child)];
  // Hacky support
  | TestLit(_) => [] // Shouldn't occur
  | Prj(d, i) => go(ApBuiltin("prj", [IntLit(i), d]))
  };
}
and infix_pat = (d1, s, d2, p) =>
  go_pat(d1) @ [atomic_operator(s, p, Pat)] @ go_pat(d2)
and go_pat = (p: DHPat.t) => {
  let prec = DHDoc_Pat.precedence(p);
  switch (p) {
  | EmptyHole(_) => [hole()]
  | NonEmptyHole(_, _, _, p) => go_pat(p)
  | Wild => [atomic_operand(Pat, "_")]
  | ExpandingKeyword(_, _, kw) => [
      atomic_operand(Pat, ExpandingKeyword.to_string(kw)),
    ]
  | BoolLit(b) => [atomic_operand(Pat, string_of_bool(b))]
  | IntLit(i) => [atomic_operand(Pat, string_of_int(i))]
  | FloatLit(f) => [atomic_operand(Pat, string_of_float(f))]
  | StringLit(s) => [atomic_operand(Pat, Form.string_quote(s))]
  | Var(s)
  | Constructor(s)
  | BadConstructor(_, _, s)
  | InvalidText(_, _, s) => [atomic_operand(Pat, s)]
  | Ap(p1, p2) => go_pat(p1) @ [ap_tile(Pat, go_pat(p2))]
  | ListLit(_, []) => [atomic_operand(Pat, "[]")]
  | ListLit(_, ps) => [listlit_tile(tuple(Pat, go_pat, ps))]
  | Cons(p1, p2) =>
    go_pat(p1) @ [atomic_operator("::", prec, Pat)] @ go_pat(p2)
  | Tuple([]) => [atomic_operand(Pat, "()")]
  | Tuple(ps) => [parens_tile(Pat, tuple(Pat, go_pat, ps))]
  };
};
