open Haz3lcore;

let mk_tile = (fam: Form.family, sort: Sort.t) =>
  Piece.mk_tile((Form.Compound(fam), sort));
let mk_monotile = (fam, sort) => mk_tile(fam, sort, []);

let cons_exp = () => mk_monotile(Cons, Exp);
let list_concat_exp = () => mk_monotile(ListConcat, Exp);
let tuple_extension_exp = () => mk_monotile(TupleExtension, Exp);
let cons_pat = () => mk_monotile(Cons, Pat);
let seq = () => mk_monotile(CellJoin, Exp);
/* Placeholder tokens must be operands: an operator-shaped token here
 * (e.g. ASCII "..." = TupleExtension) makes the docs segment nonconvex
 * and crashes the skel. Failfast at init; use Token.ellipsis for
 * ellipsis placeholders. */
let operand = (sort, v) => {
  let p = Piece.mk_tile(Form.classify_label(sort, [v]), []);
  switch (p) {
  | Tile(t) =>
    let (l, r) = Tile.mold(t).nibs;
    if (!Nib.Shape.equal(l.shape, Convex)
        || !Nib.Shape.equal(r.shape, Convex)) {
      failwith("Example: placeholder is not an operand: " ++ v);
    };
  | _ => ()
  };
  p;
};
/* Operator glyphs shown between operands keep their classified
 * (infix) molds */
let operator = (sort, v) =>
  Piece.mk_tile(Form.classify_label(sort, [v]), []);
let exp = v => operand(Sort.Exp, v);
let pat = v => operand(Sort.Pat, v);
let typ = t => operand(Sort.Typ, t);
let tpat = v => operand(Sort.TPat, v);
let mk_list_exp = mk_tile(ListLit, Exp);
let mk_list_pat = mk_tile(ListLit, Pat);
let mk_list_typ = mk_tile(ListLit, Typ);
let mk_poly = mk_tile(Poly, Typ);
let mk_rec = mk_tile(Rec, Typ);
let arrow = () => mk_monotile(TypeArrow, Typ);
let unary_minus = () => mk_monotile(UnaryMinus, Exp);
let unary_not = () => mk_monotile(Not, Exp);
let plus = () => mk_monotile(Plus, Exp);
let minus = () => mk_monotile(Minus, Exp);
let times = () => mk_monotile(Times, Exp);
let power = () => mk_monotile(Power, Exp);
let divide = () => mk_monotile(Divide, Exp);
let equals = () => mk_monotile(Equals, Exp);
let not_equals = () => mk_monotile(NotEquals, Exp);
let lt = () => mk_monotile(Lt, Exp);
let lte = () => mk_monotile(Lte, Exp);
let gt = () => mk_monotile(Gt, Exp);
let gte = () => mk_monotile(Gte, Exp);
let fplus = () => mk_monotile(FPlus, Exp);
let fminus = () => mk_monotile(FMinus, Exp);
let ftimes = () => mk_monotile(FTimes, Exp);
let fpower = () => mk_monotile(FPower, Exp);
let fdivide = () => mk_monotile(FDivide, Exp);
let fequals = () => mk_monotile(FEquals, Exp);
let fnot_equals = () => mk_monotile(FNotEquals, Exp);
let flt = () => mk_monotile(FLt, Exp);
let flte = () => mk_monotile(FLte, Exp);
let fgt = () => mk_monotile(FGt, Exp);
let fgte = () => mk_monotile(FGte, Exp);
let sconcat = () => mk_monotile(StringConcat, Exp);
let logical_and = () => mk_monotile(LogicalAnd, Exp);
let logical_or = () => mk_monotile(LogicalOr, Exp);
let comma_exp = () => mk_monotile(Comma, Exp);
let comma_pat = () => mk_monotile(Comma, Pat);
let comma_typ = () => mk_monotile(Comma, Typ);
let pipeline = () => mk_monotile(Pipeline, Exp);
let labeled_exp = () => mk_monotile(TupleLabeled, Exp);
let labeled_pat = () => mk_monotile(TupleLabeled, Pat);
let labeled_typ = () => mk_monotile(TupleLabeled, Typ);
let dot_typ = () => mk_monotile(Dot, Typ);
let ascription_exp = () => mk_monotile(TypeAsc, Exp);
let deferral = () => exp("_");
let typeann = () => mk_monotile(TypeAsc, Pat);
let mk_typabs = mk_tile(TypAbs, Exp);
let mk_typfun = mk_tile(TypFun, Typ);
let mk_ap_typ = mk_tile(Ap, Typ);
let mk_ap_tpat = mk_tile(Ap, TPat);
let comma_tpat = () => mk_monotile(Comma, TPat);
let mk_fun = mk_tile(Fun, Exp);
let mk_ap_exp_typ = mk_tile(ApExpTyp, Exp);
let mk_fix = mk_tile(Fix, Exp);
let mk_ap_exp = mk_tile(Ap, Exp);
let mk_ap_pat = mk_tile(Ap, Pat);
let mk_let = mk_tile(Let, Exp);
let mk_use = mk_tile(Use, Exp);
let mk_tyalias = mk_tile(TypeAlias, Exp);
let mk_if = mk_tile(If, Exp);
let mk_test = mk_tile(Test, Exp);
let mk_hinted_test = mk_tile(HintedTest, Exp);
let mk_case = mk_tile(Case, Exp);
let mk_rule = mk_tile(Rule, Rul);
let mk_hide = mk_tile(FilterHide, Exp);
let mk_eval = mk_tile(FilterEval, Exp);
let mk_pause = mk_tile(FilterPause, Exp);
let mk_debug = mk_tile(FilterDebug, Exp);
let mk_theorem = mk_tile(Theorem, Exp);
let mk_forall = mk_tile(Forall, Exp);
let mk_proof_of = mk_tile(ProofOf, Typ);
let linebreak = () => Piece.Secondary(Secondary.mk_newline(Id.mk()));
let space = () => Piece.Secondary(Secondary.mk_space(Id.mk()));

/* Examples parse on first display, not at startup */
let mk_example = str =>
  lazy(
    switch (Parser.to_segment(str, ~root=Exp)) {
    | None => []
    | Some(seg) => Indentation.reindent_segment(seg)
    }
  );

/* Int param below should be ~ width of sidebar */
let abbreviate = Token.abbreviate(20);

/* [n] placeholder pieces named after [name] (`a`, or `a1`, `a2`, … for
   more than one) separated by commas, as in a binder list `a1, a2` or an
   argument list `T1, T2`; returns the segment and the placeholders. */
let comma_list =
    (~sep: unit => Piece.t, mk: string => Piece.t, name: string, n: int)
    : (Segment.t, list(Piece.t)) => {
  let pieces =
    n == 1
      ? [mk(name)]
      : List.init(n, ~f=i => mk(Printf.sprintf("%s%d", name, i + 1)));
  let seg =
    pieces
    |> List.mapi(~f=(i, p) => i == 0 ? [p] : [sep(), space(), p])
    |> List.concat;
  (seg, pieces);
};
let tpat_list = comma_list(~sep=comma_tpat, tpat);
let typ_list = comma_list(~sep=comma_typ, typ);

/* Colorings pairing placeholders with the terms they stand for, as far
   as both lists go. */
let rec zip_colorings =
        (pieces: list(Piece.t), ids: list(Id.t)): list((Id.t, Id.t)) =>
  switch (pieces, ids) {
  | ([p, ...ps], [id, ...ids]) => [
      (Piece.id(p), id),
      ...zip_colorings(ps, ids),
    ]
  | _ => []
  };

/* `[*label*](id)` links, comma separated */
let links = (label: string, ids: list(Id.t)): string =>
  ids
  |> List.map(~f=id =>
       Printf.sprintf("[*%s*](%s)", label, Id.to_string(id))
     )
  |> String.concat(~sep=", ");

/* Links labelled with the names they stand for, `a` and `b` */
let named_links = (named: list((string, Id.t))): string => {
  let link = ((name, id)) =>
    Printf.sprintf("[*%s*](%s)", name, Id.to_string(id));
  switch (List.rev(named)) {
  | [] => ""
  | [last] => link(last)
  | [last, ...rest] =>
    String.concat(~sep=", ", List.rev_map(~f=link, rest))
    ++ " and "
    ++ link(last)
  };
};
