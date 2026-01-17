open Haz3lcore;

let mk_tile = Piece.mk_tile;
let mk_monotile = mk_tile(_, []);

let cons_exp = () => mk_monotile(Form.get(ConsExp));
let list_concat_exp = () => mk_monotile(Form.get(ListConcat));
let tuple_extension_exp = () => mk_monotile(Form.get(TupleExtension));
let cons_pat = () => mk_monotile(Form.get(ConsPat));
let seq = () => mk_monotile(Form.get(CellJoin));
let exp = v => mk_monotile(Form.mk_atom_op(Exp, v));
let pat = v => mk_monotile(Form.mk_atom_op(Pat, v));
let typ = t => mk_monotile(Form.mk_atom_op(Typ, t));
let tpat = v => mk_monotile(Form.mk_atom_op(TPat, v));
let mk_parens_exp = mk_tile(Form.get(ParensExp));
let mk_parens_pat = mk_tile(Form.get(ParensPat));
let mk_parens_typ = mk_tile(Form.get(ParensTyp));
let mk_list_exp = mk_tile(Form.get(ListLitExp));
let mk_list_pat = mk_tile(Form.get(ListLitPat));
let mk_list_typ = mk_tile(Form.get(ListTyp));
let mk_poly = mk_tile(Form.get(Poly));
let mk_rec = mk_tile(Form.get(Rec));
let arrow = () => mk_monotile(Form.get(TypeArrow));
let unary_minus = () => mk_monotile(Form.get(UnaryMinus));
let unary_not = () => mk_monotile(Form.get(Not));
let plus = () => mk_monotile(Form.get(Plus));
let minus = () => mk_monotile(Form.get(Minus));
let times = () => mk_monotile(Form.get(Times));
let power = () => mk_monotile(Form.get(Power));
let divide = () => mk_monotile(Form.get(Divide));
let equals = () => mk_monotile(Form.get(Equals));
let not_equals = () => mk_monotile(Form.get(NotEquals));
let lt = () => mk_monotile(Form.get(Lt));
let lte = () => mk_monotile(Form.get(Lte));
let gt = () => mk_monotile(Form.get(Gt));
let gte = () => mk_monotile(Form.get(Gte));
let fplus = () => mk_monotile(Form.get(FPlus));
let fminus = () => mk_monotile(Form.get(FMinus));
let ftimes = () => mk_monotile(Form.get(FTimes));
let fpower = () => mk_monotile(Form.get(FPower));
let fdivide = () => mk_monotile(Form.get(FDivide));
let fequals = () => mk_monotile(Form.get(FEquals));
let fnot_equals = () => mk_monotile(Form.get(FNotEquals));
let flt = () => mk_monotile(Form.get(FLt));
let flte = () => mk_monotile(Form.get(FLte));
let fgt = () => mk_monotile(Form.get(FGt));
let fgte = () => mk_monotile(Form.get(FGte));
let sequals = () => mk_monotile(Form.get(StringEquals));
let sconcat = () => mk_monotile(Form.get(StringConcat));
let logical_and = () => mk_monotile(Form.get(LogicalAnd));
let logical_or = () => mk_monotile(Form.get(LogicalOr));
let comma_exp = () => mk_monotile(Form.get(CommaExp));
let comma_pat = () => mk_monotile(Form.get(CommaPat));
let comma_typ = () => mk_monotile(Form.get(CommaTyp));
let pipeline = () => mk_monotile(Form.get(Pipeline));
let labeled_exp = () => mk_monotile(Form.get(TupleLabeledExp));
let labeled_pat = () => mk_monotile(Form.get(TupleLabeledPat));
let labeled_typ = () => mk_monotile(Form.get(TupleLabeledTyp));
let dot_exp = () => mk_monotile(Form.get(DotExp));
let dot_typ = () => mk_monotile(Form.get(DotTyp));
let ascription_exp = () => mk_monotile(Form.get(TypeAsc));
let nil = () => exp("[]");
let deferral = () => exp("_");
let typeann = () => mk_monotile(Form.get(Typeann));
let mk_typfun = mk_tile(Form.get(TypFun));
let mk_fun = mk_tile(Form.get(Fun));
let mk_ap_exp_typ = mk_tile(Form.get(ApExpTyp));
let mk_fix = mk_tile(Form.get(Fix));
let mk_ap_exp = mk_tile(Form.get(ApExp));
let mk_ap_pat = mk_tile(Form.get(ApPat));
let mk_let = mk_tile(Form.get(Let));
let mk_use = mk_tile(Form.get(Use));
let mk_tyalias = mk_tile(Form.get(TypeAlias));
let mk_if = mk_tile(Form.get(If));
let mk_test = mk_tile(Form.get(Test));
let mk_hinted_test = mk_tile(Form.get(HintedTest));
let mk_case = mk_tile(Form.get(Case));
let mk_rule = mk_tile(Form.get(Rule));
let mk_hide = mk_tile(Form.get(FilterHide));
let mk_eval = mk_tile(Form.get(FilterEval));
let mk_pause = mk_tile(Form.get(FilterPause));
let mk_debug = mk_tile(Form.get(FilterDebug));
let mk_unquote = mk_tile(Form.get(Unquote));
let mk_theorem = mk_tile(Form.get(Theorem));
let mk_proof_object = mk_tile(Form.get(ProofObject));
let mk_forall = mk_tile(Form.get(Forall));
let mk_proof_of = mk_tile(Form.get(ProofOf));
let linebreak = () => Piece.Secondary(Secondary.mk_newline(Id.mk()));
let space = () => Piece.Secondary(Secondary.mk_space(Id.mk()));

let mk_example = str => {
  switch (Parser.to_segment(str)) {
  | None => []
  | Some(seg) => seg
  };
};

/* Int param below should be ~ width of sidebar */
let abbreviate = Token.abbreviate(20);
