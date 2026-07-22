open Haz3lcore;

let mk_tile = (form: Form.compound_form) =>
  Piece.mk_tile((
    Form.Compound(Form.family_of(form)),
    Form.get(form).mold.out,
  ));
let mk_monotile = form => mk_tile(form, []);

let cons_exp = () => mk_monotile(ConsExp);
let list_concat_exp = () => mk_monotile(ListConcat);
let tuple_extension_exp = () => mk_monotile(TupleExtension);
let cons_pat = () => mk_monotile(ConsPat);
let seq = () => mk_monotile(CellJoin);
let exp = v => Piece.mk_tile(Form.classify_label(Exp, [v]), []);
let pat = v => Piece.mk_tile(Form.classify_label(Pat, [v]), []);
let typ = t => Piece.mk_tile(Form.classify_label(Typ, [t]), []);
let tpat = v => Piece.mk_tile(Form.classify_label(TPat, [v]), []);
let mk_parens_exp = mk_tile(ParensExp);
let mk_parens_pat = mk_tile(ParensPat);
let mk_parens_typ = mk_tile(ParensTyp);
let mk_list_exp = mk_tile(ListLitExp);
let mk_list_pat = mk_tile(ListLitPat);
let mk_list_typ = mk_tile(ListTyp);
let mk_poly = mk_tile(Poly);
let mk_rec = mk_tile(Rec);
let arrow = () => mk_monotile(TypeArrow);
let unary_minus = () => mk_monotile(UnaryMinus);
let unary_not = () => mk_monotile(Not);
let plus = () => mk_monotile(Plus);
let minus = () => mk_monotile(Minus);
let times = () => mk_monotile(Times);
let power = () => mk_monotile(Power);
let divide = () => mk_monotile(Divide);
let equals = () => mk_monotile(Equals);
let not_equals = () => mk_monotile(NotEquals);
let lt = () => mk_monotile(Lt);
let lte = () => mk_monotile(Lte);
let gt = () => mk_monotile(Gt);
let gte = () => mk_monotile(Gte);
let fplus = () => mk_monotile(FPlus);
let fminus = () => mk_monotile(FMinus);
let ftimes = () => mk_monotile(FTimes);
let fpower = () => mk_monotile(FPower);
let fdivide = () => mk_monotile(FDivide);
let fequals = () => mk_monotile(FEquals);
let fnot_equals = () => mk_monotile(FNotEquals);
let flt = () => mk_monotile(FLt);
let flte = () => mk_monotile(FLte);
let fgt = () => mk_monotile(FGt);
let fgte = () => mk_monotile(FGte);
let sconcat = () => mk_monotile(StringConcat);
let logical_and = () => mk_monotile(LogicalAnd);
let logical_or = () => mk_monotile(LogicalOr);
let comma_exp = () => mk_monotile(CommaExp);
let comma_pat = () => mk_monotile(CommaPat);
let comma_typ = () => mk_monotile(CommaTyp);
let pipeline = () => mk_monotile(Pipeline);
let labeled_exp = () => mk_monotile(TupleLabeledExp);
let labeled_pat = () => mk_monotile(TupleLabeledPat);
let labeled_typ = () => mk_monotile(TupleLabeledTyp);
let dot_exp = () => mk_monotile(DotExp);
let dot_typ = () => mk_monotile(DotTyp);
let ascription_exp = () => mk_monotile(TypeAsc);
let nil = () => exp("[]");
let deferral = () => exp("_");
let typeann = () => mk_monotile(Typeann);
let mk_typfun = mk_tile(TypFun);
let mk_fun = mk_tile(Fun);
let mk_ap_exp_typ = mk_tile(ApExpTyp);
let mk_fix = mk_tile(Fix);
let mk_ap_exp = mk_tile(ApExp);
let mk_ap_pat = mk_tile(ApPat);
let mk_let = mk_tile(Let);
let mk_use = mk_tile(Use);
let mk_tyalias = mk_tile(TypeAlias);
let mk_if = mk_tile(If);
let mk_test = mk_tile(Test);
let mk_hinted_test = mk_tile(HintedTest);
let mk_case = mk_tile(Case);
let mk_rule = mk_tile(Rule);
let mk_hide = mk_tile(FilterHide);
let mk_eval = mk_tile(FilterEval);
let mk_pause = mk_tile(FilterPause);
let mk_debug = mk_tile(FilterDebug);
let mk_theorem = mk_tile(Theorem);
let mk_proof_object = mk_tile(ProofObject);
let mk_forall = mk_tile(Forall);
let mk_proof_of = mk_tile(ProofOf);
let linebreak = () => Piece.Secondary(Secondary.mk_newline(Id.mk()));
let space = () => Piece.Secondary(Secondary.mk_space(Id.mk()));

let mk_example = str => {
  switch (Parser.to_segment(str, ~root=Exp)) {
  | None => []
  | Some(seg) => seg
  };
};

/* Int param below should be ~ width of sidebar */
let abbreviate = Token.abbreviate(20);
