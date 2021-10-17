[@deriving sexp]
type generator = Suggestion.generator;
[@deriving sexp]
type generator' = Suggestion.generator';

let mk_operand_suggestion =
    (
      ~strategy: Suggestion.typ_operand_strategy,
      ~operand: UHTyp.operand,
      ci: CursorInfo.t,
    )
    : Suggestion.t => {
  let report = SuggestionReportTyp.mk_operand_report(operand, ci);
  ReplaceTypOperand({operand, typ_operand_strategy: strategy, report});
};

let mk_operand_suggestion_from_uhtyp = (~strategy, ~uhtyp, ci) =>
  mk_operand_suggestion(~strategy, ~operand=UHTyp.Parenthesized(uhtyp), ci);

let hole_operand = UHTyp.Hole;

let seq_to_uhtyp = seq => seq |> UHTyp.mk_OpSeq;

let rec mk_n_seq = (op: Operators_Typ.t, n: int) =>
  switch (n) {
  | 0
  | 1 => failwith("mk_n_seq: n must be 2 or more")
  | 2 => Seq.S(hole_operand, A(op, S(hole_operand, E)))
  | _ => Seq.S(hole_operand, A(op, mk_n_seq(op, n - 1)))
  };

let mk_empty_hole_suggestion: generator' =
  mk_operand_suggestion(~strategy=Delete, ~operand=hole_operand);

let mk_lit_suggestion = mk_operand_suggestion(~strategy=InsertLit);

let mk_insert_lit_suggestions: generator =
  ci => [
    mk_lit_suggestion(~operand=Int, ci),
    mk_lit_suggestion(~operand=Float, ci),
    mk_lit_suggestion(~operand=Bool, ci),
    mk_lit_suggestion(~operand=Unit, ci),
    mk_lit_suggestion(
      ~operand=UHTyp.Parenthesized(UHTyp.contract(Arrow(Hole, Hole))),
      ci,
    ),
    mk_lit_suggestion(
      ~operand=UHTyp.Parenthesized(UHTyp.contract(Sum(Hole, Hole))),
      ci,
    ),
    mk_lit_suggestion(
      ~operand=UHTyp.Parenthesized(UHTyp.contract(Prod([Hole, Hole]))),
      ci,
    ),
  ];

let mk_delete_suggestions: generator = ci => [mk_empty_hole_suggestion(ci)];

let typ_operand_generators = [
  mk_insert_lit_suggestions,
  mk_delete_suggestions,
];

let mk: generator = Suggestion.generate(List.rev(typ_operand_generators));
