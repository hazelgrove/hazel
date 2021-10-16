[@deriving sexp]
type generator = Suggestion.generator;
[@deriving sexp]
type generator' = Suggestion.generator';

let mk_operand_suggestion =
    (
      ~strategy: Suggestion.pat_operand_strategy,
      ~operand: UHPat.operand,
      ci: CursorInfo.t,
    )
    : Suggestion.t => {
  let report = SuggestionReportPat.mk_pat_operand_report(operand, ci);
  ReplacePatOperand({operand, pat_operand_strategy: strategy, report});
};

let mk_operand_suggestion_from_uhpat = (~strategy, ~uhpat, ci) =>
  mk_operand_suggestion(~strategy, ~operand=UHPat.Parenthesized(uhpat), ci);

let hole_operand = UHPat.EmptyHole(0);

let mk_inj = (side, operand: UHPat.operand) =>
  UHPat.inj(side, OpSeq.wrap(operand));

let seq_to_uhpat = seq => seq |> UHPat.mk_OpSeq;

let rec mk_n_seq = (op: Operators_Pat.t, n: int) =>
  switch (n) {
  | 0
  | 1 => failwith("mk_n_seq: n must be 2 or more")
  | 2 => Seq.S(hole_operand, A(op, S(hole_operand, E)))
  | _ => Seq.S(hole_operand, A(op, mk_n_seq(op, n - 1)))
  };

let mk_empty_hole_suggestion: generator' =
  mk_operand_suggestion(~strategy=Delete, ~operand=hole_operand);

let mk_lit_suggestion = mk_operand_suggestion(~strategy=InsertLit);

let mk_nil_list_suggestion: generator' =
  mk_lit_suggestion(~operand=UHPat.listnil());

let mk_bool_lit_suggestion: bool => generator' =
  b => mk_lit_suggestion(~operand=UHPat.boollit(b));

let mk_int_lit_suggestion: string => generator' =
  s => mk_lit_suggestion(~operand=UHPat.intlit(s));

let mk_float_lit_suggestion: string => generator' =
  s => mk_lit_suggestion(~operand=UHPat.floatlit(s));

let mk_inj_suggestion: (InjSide.t, UHPat.operand) => generator' =
  (side, operand) =>
    mk_operand_suggestion(
      ~strategy=InsertLit,
      ~operand=mk_inj(side, operand),
    );

let mk_pair_suggestion: generator' =
  mk_operand_suggestion_from_uhpat(
    ~strategy=InsertLit,
    ~uhpat=seq_to_uhpat(mk_n_seq(Operators_Pat.Comma, 2)),
  );

let mk_list_suggestion: generator' =
  mk_operand_suggestion_from_uhpat(
    ~strategy=InsertLit,
    ~uhpat=seq_to_uhpat(mk_n_seq(Operators_Pat.Cons, 2)),
  );

let rec mk_constructors = (expected_ty: HTyp.t, ci) =>
  // add ability to insert triv
  switch (expected_ty) {
  | Bool => [
      mk_bool_lit_suggestion(true, ci),
      mk_bool_lit_suggestion(false, ci),
    ]
  | Int => [mk_int_lit_suggestion("1", ci)]
  | Float => [mk_float_lit_suggestion("1.", ci)]
  | List(_) => [mk_nil_list_suggestion(ci), mk_list_suggestion(ci)]
  | Sum(_, _) => [
      mk_inj_suggestion(L, hole_operand, ci),
      mk_inj_suggestion(R, hole_operand, ci),
    ]
  | Prod(_) => [mk_pair_suggestion(ci)] // TODO: n-tuples
  | Arrow(_, _) => []
  | Hole =>
    // add new types here for synthetic position
    // ordering here is becomes default for UI
    HTyp.[
      Bool,
      Int,
      Float,
      List(Hole),
      Sum(Hole, Hole),
      Prod([]),
      Arrow(Hole, Hole),
    ]
    |> List.map(ty => mk_constructors(ty, ci))
    |> List.concat
  };
let mk_insert_lit_suggestions: generator =
  ci => mk_constructors(ci.expected_ty, ci);

let mk_delete_suggestions: generator = ci => [mk_empty_hole_suggestion(ci)];
let pat_operand_generators = [
  mk_insert_lit_suggestions,
  mk_delete_suggestions,
];

let mk: generator = Suggestion.generate(List.rev(pat_operand_generators));
