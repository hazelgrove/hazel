open Haz3lcore;

let mk_tile = Piece.mk_tile;
let mk_monotile = mk_tile(_, []);

let cons_exp = () => mk_monotile(FormId.Form(ConsExp));
let list_concat_exp = () => mk_monotile(FormId.Form(ListConcat));
let tuple_extension_exp = () => mk_monotile(FormId.Form(TupleExtension));
let cons_pat = () => mk_monotile(FormId.Form(ConsPat));
let seq = () => mk_monotile(FormId.Form(CellJoin));
let exp = v => mk_monotile(Form.classify_label(Exp, [v]));
let pat = v => mk_monotile(Form.classify_label(Pat, [v]));
let typ = t => mk_monotile(Form.classify_label(Typ, [t]));
let tpat = v => mk_monotile(Form.classify_label(TPat, [v]));
let mk_parens_exp = mk_tile(FormId.Form(ParensExp));
let mk_parens_pat = mk_tile(FormId.Form(ParensPat));
let mk_parens_typ = mk_tile(FormId.Form(ParensTyp));
let mk_list_exp = mk_tile(FormId.Form(ListLitExp));
let mk_list_pat = mk_tile(FormId.Form(ListLitPat));
let mk_list_typ = mk_tile(FormId.Form(ListTyp));
let mk_poly = mk_tile(FormId.Form(Poly));
let mk_rec = mk_tile(FormId.Form(Rec));
let arrow = () => mk_monotile(FormId.Form(TypeArrow));
let unary_minus = () => mk_monotile(FormId.Form(UnaryMinus));
let unary_not = () => mk_monotile(FormId.Form(Not));
let plus = () => mk_monotile(FormId.Form(Plus));
let minus = () => mk_monotile(FormId.Form(Minus));
let times = () => mk_monotile(FormId.Form(Times));
let power = () => mk_monotile(FormId.Form(Power));
let divide = () => mk_monotile(FormId.Form(Divide));
let equals = () => mk_monotile(FormId.Form(Equals));
let not_equals = () => mk_monotile(FormId.Form(NotEquals));
let lt = () => mk_monotile(FormId.Form(Lt));
let lte = () => mk_monotile(FormId.Form(Lte));
let gt = () => mk_monotile(FormId.Form(Gt));
let gte = () => mk_monotile(FormId.Form(Gte));
let fplus = () => mk_monotile(FormId.Form(FPlus));
let fminus = () => mk_monotile(FormId.Form(FMinus));
let ftimes = () => mk_monotile(FormId.Form(FTimes));
let fpower = () => mk_monotile(FormId.Form(FPower));
let fdivide = () => mk_monotile(FormId.Form(FDivide));
let fequals = () => mk_monotile(FormId.Form(FEquals));
let fnot_equals = () => mk_monotile(FormId.Form(FNotEquals));
let flt = () => mk_monotile(FormId.Form(FLt));
let flte = () => mk_monotile(FormId.Form(FLte));
let fgt = () => mk_monotile(FormId.Form(FGt));
let fgte = () => mk_monotile(FormId.Form(FGte));
let sconcat = () => mk_monotile(FormId.Form(StringConcat));
let logical_and = () => mk_monotile(FormId.Form(LogicalAnd));
let logical_or = () => mk_monotile(FormId.Form(LogicalOr));
let comma_exp = () => mk_monotile(FormId.Form(CommaExp));
let comma_pat = () => mk_monotile(FormId.Form(CommaPat));
let comma_typ = () => mk_monotile(FormId.Form(CommaTyp));
let pipeline = () => mk_monotile(FormId.Form(Pipeline));
let labeled_exp = () => mk_monotile(FormId.Form(TupleLabeledExp));
let labeled_pat = () => mk_monotile(FormId.Form(TupleLabeledPat));
let labeled_typ = () => mk_monotile(FormId.Form(TupleLabeledTyp));
let dot_exp = () => mk_monotile(FormId.Form(DotExp));
let dot_typ = () => mk_monotile(FormId.Form(DotTyp));
let ascription_exp = () => mk_monotile(FormId.Form(TypeAsc));
let nil = () => exp("[]");
let deferral = () => exp("_");
let typeann = () => mk_monotile(FormId.Form(Typeann));
let mk_typfun = mk_tile(FormId.Form(TypFun));
let mk_fun = mk_tile(FormId.Form(Fun));
let mk_ap_exp_typ = mk_tile(FormId.Form(ApExpTyp));
let mk_fix = mk_tile(FormId.Form(Fix));
let mk_ap_exp = mk_tile(FormId.Form(ApExp));
let mk_ap_pat = mk_tile(FormId.Form(ApPat));
let mk_let = mk_tile(FormId.Form(Let));
let mk_use = mk_tile(FormId.Form(Use));
let mk_tyalias = mk_tile(FormId.Form(TypeAlias));
let mk_if = mk_tile(FormId.Form(If));
let mk_test = mk_tile(FormId.Form(Test));
let mk_hinted_test = mk_tile(FormId.Form(HintedTest));
let mk_case = mk_tile(FormId.Form(Case));
let mk_rule = mk_tile(FormId.Form(Rule));
let mk_hide = mk_tile(FormId.Form(FilterHide));
let mk_eval = mk_tile(FormId.Form(FilterEval));
let mk_pause = mk_tile(FormId.Form(FilterPause));
let mk_debug = mk_tile(FormId.Form(FilterDebug));
let mk_theorem = mk_tile(FormId.Form(Theorem));
let mk_proof_object = mk_tile(FormId.Form(ProofObject));
let mk_forall = mk_tile(FormId.Form(Forall));
let mk_proof_of = mk_tile(FormId.Form(ProofOf));
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
