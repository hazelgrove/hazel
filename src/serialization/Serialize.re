open Semantics.Core;
open Format;
open LangUtil;
let ensure_well_typed_before_serialization = uhexp =>
  switch (UHExp.syn((), Ctx.empty, uhexp)) {
  | None => raise(IllFormed(uhexp))
  | _ => uhexp
  };
let string_of_tyop = op =>
  switch (op) {
  | UHTyp.Arrow => " ->"
  | UHTyp.Sum => " |"
  };
let string_of_expop = op =>
  switch (op) {
  | UHExp.Plus => " +"
  | UHExp.Times => " *"
  | UHExp.Space => ""
  };
let serialize = (~fmtr=std_formatter, ~line_length=100, ~indent=2, uhexp) => {
  pp_set_margin(fmtr, line_length);
  let rec print_opseq' = (fmtr, print_term, string_of_op, seq) => {
    let print_rest = (op, tm2) =>
      fprintf(fmtr, "%s@ %a", string_of_op(op), print_term, tm2);
    switch (seq) {
    | OperatorSeq.ExpOpExp(tm1, op, tm2) =>
      print_term(fmtr, tm1);
      print_rest(op, tm2);
    | OperatorSeq.SeqOpExp(seq', op, tm2) =>
      print_opseq'(fmtr, print_term, string_of_op, seq');
      print_rest(op, tm2);
    };
  };

  let print_opseq = (fmtr, print_term, string_of_op, seq) =>
    fprintf(fmtr, "@[<hv %d>%t@]", indent, fmtr =>
      print_opseq'(fmtr, print_term, string_of_op, seq)
    );

  let print_parenthesized = (fmtr, print_value) =>
    fprintf(fmtr, "@[<hov %d>(%t@,)@]", indent, print_value);
  let rec print_htyp = (fmtr, tau) =>
    switch (tau) {
    | UHTyp.Parenthesized(tau1) =>
      print_parenthesized(fmtr, fmtr => print_htyp(fmtr, tau1))
    | UHTyp.Num => fprintf(fmtr, "num")
    | UHTyp.Hole => fprintf(fmtr, "{}")
    | UHTyp.OpSeq(skel, seq) =>
      print_opseq(fmtr, print_htyp, string_of_tyop, seq)
    };

  let rec print_uhexp = (fmtr, e) =>
    switch (e) {
    | UHExp.Parenthesized(e') =>
      print_parenthesized(fmtr, fmtr => print_uhexp(fmtr, e'))
    | UHExp.Tm(_, tm) =>
      switch (tm) {
      | UHExp.Asc(e', tau) =>
        fprintf(
          fmtr,
          "@[<hov %d>%a :@ %a@]",
          indent,
          print_uhexp,
          e',
          print_htyp,
          tau,
        )
      | UHExp.Var(x) => fprintf(fmtr, "%s", x)
      | UHExp.Let(x, e1, e2) =>
        fprintf(
          fmtr,
          "@[<v>@[<hov %d>let %s =@ %a in@]@ %a@]",
          indent,
          x,
          print_uhexp,
          e1,
          print_uhexp,
          e2,
        )
      | UHExp.Lam(x, e') =>
        fprintf(
          fmtr,
          "@[<hov %d>lambda %s.@,%a@]",
          indent,
          x,
          print_uhexp,
          e',
        )
      | UHExp.NumLit(n) => fprintf(fmtr, "%d", n)
      | UHExp.Inj(side, e') =>
        fprintf(
          fmtr,
          "@[<%d>inj[%s](@,%a@,)@]",
          indent,
          string_of_side(side),
          print_uhexp,
          e',
        )
      | UHExp.Case(e', (vL, eL), (vR, eR)) =>
        let print_side = (fmtr, side_str, side_v, side_exp) =>
          fprintf(
            fmtr,
            "@[<hov %d>%s(%s) =>@ %a@]",
            indent,
            side_str,
            side_v,
            print_uhexp,
            side_exp,
          );

        fprintf(
          fmtr,
          "@[<v>case %a@ %t@ %t@]",
          print_uhexp,
          e',
          fmtr => print_side(fmtr, "L", vL, eL),
          fmtr => print_side(fmtr, "R", vR, eR),
        );
      | UHExp.EmptyHole(u) => fprintf(fmtr, "{}")
      | UHExp.OpSeq(skel, seq) =>
        print_opseq(fmtr, print_uhexp, string_of_expop, seq)
      }
    };

  print_uhexp(fmtr, ensure_well_typed_before_serialization(uhexp));
  pp_print_newline(fmtr, ());
};

let string_of_uhexp = hexp => {
  let buf = Buffer.create(32);
  let fmtr = formatter_of_buffer(buf);
  serialize(~fmtr, hexp);
  pp_print_flush(fmtr, ());
  Buffer.contents(buf);
};
