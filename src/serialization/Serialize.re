open SemanticsCore;
open Format;
open LangUtil;
let ensure_well_typed_before_serialization = uhexp =>
  switch (UHExp.syn((), (Ctx.empty, PaletteCtx.empty), uhexp)) {
  | None => raise(IllFormed(uhexp))
  | _ => uhexp
  };
let string_of_tyop = op =>
  switch (op) {
  | UHTyp.Arrow => " ->"
  | UHTyp.Sum => " |"
  | UHTyp.Prod => " &"
  };
let string_of_expop = op =>
  switch (op) {
  | UHExp.Plus => " +"
  | UHExp.Times => " *"
  | UHExp.LessThan => " <"
  | UHExp.Space => ""
  | UHExp.Comma => ","
  | UHExp.Cons => " ::"
  };
let string_of_patop = op =>
  switch (op) {
  | UHPat.Comma => ","
  | UHPat.Space => ""
  | UHPat.Cons => " ::"
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

  let print_list_nil = (fmtr) =>
    fprintf(fmtr, "[]");

  let rec print_uhtyp = (fmtr, tau) =>
    switch (tau) {
    | UHTyp.Parenthesized(tau1) =>
      print_parenthesized(fmtr, fmtr => print_uhtyp(fmtr, tau1))
    | UHTyp.Num => fprintf(fmtr, "num")
    | UHTyp.Bool => fprintf(fmtr, "bool")
    | UHTyp.List(t) =>
      fprintf(fmtr, "@[<hov %d>[%a@,]@]", indent, print_uhtyp, t);
    | UHTyp.Hole => fprintf(fmtr, "{}")
    | UHTyp.OpSeq(_, seq) =>
      print_opseq(fmtr, print_uhtyp, string_of_tyop, seq)
    };

  let rec print_uhpat = (fmtr, p) =>
          switch(p) {
                  | UHPat.Parenthesized(p') =>
                    print_parenthesized(fmtr, fmtr => print_uhpat(fmtr, p'))
                  | UHPat.Pat(_, pat) =>
                    switch(pat) {
                      | UHPat.EmptyHole(_) =>
                        fprintf(fmtr, "{}")
                      | UHPat.Wild =>
                      fprintf(fmtr, "_")
                      | UHPat.Var(x) =>
                      fprintf(fmtr, "%s", x)
                      | UHPat.NumLit(n) =>
                      fprintf(fmtr, "%d", n)
                      | UHPat.BoolLit(b) =>
                      fprintf(fmtr, if (b) {"true"} else {"false"})
                      | UHPat.Inj(s, p') =>
                      fprintf(
          fmtr,
          "@[<%d>inj[%s](@,%a@,)@]",
          indent,
          string_of_side(side),
          print_uhpat,
          p'
        )
                      | UHPat.ListNil =>
                        print_list_nil(fmtr)
                      | UHPat.OpSeq(_, seq) =>
        print_opseq(fmtr, print_uhpat, string_of_patop, seq)
          }};

  let print_half_ann = (fmtr, annOpt)
            switch (annOpt) {
                    | None => fprintf(fmtr, "") // NOOP
                    | Some(ann) => fprintf(fmtr, "@ : %a", print_uhtyp, ann)
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
          print_uhtyp,
          tau,
        )
      | UHExp.Var(_, x) => fprintf(fmtr, "%s", x)
      | UHExp.Let(p, annOpt, e1, e2) =>
        fprintf(
          fmtr,
          "@[<v>@[<hov %d>let %a%a@ = %a in@]@ %a@]",
          indent,
          print_uhpat,
          p,
          print_half_ann,
          annOpt,
          print_uhexp,
          e1,
          print_uhexp,
          e2,
        )
      | UHExp.Lam(p, annOpt, e') =>
        fprintf(
          fmtr,
          "@[<hov %d>lambda %a%a.@ %a@]",
          indent,
          print_uhpat,
          p,
          print_half_ann,
          annOpt,
          print_uhexp,
          e',
        )
      | UHExp.NumLit(n) => fprintf(fmtr, "%d", n)
      | UHExp.BoolLit(b) =>
                      fprintf(fmtr, if (b) {"true"} else {"false"})
      | UHExp.Inj(side, e') =>
        fprintf(
          fmtr,
          "@[<%d>inj[%s](@,%a@,)@]",
          indent,
          string_of_side(side),
          print_uhexp,
          e',
        )
      | UHExp.Case(e', rules) =>
        let print_rule = (fmtr, Rule(pr, er)) =>
          fprintf(
            fmtr,
            "@[<hov %d>@ || %a =>@ %a@]",
            indent,
            print_uhpat,
            pr,
            print_uhexp,
            er
          );

        fprintf(
          fmtr,
          "@[<v>case %a%t;@]",
          print_uhexp,
          e',
          fmtr => {List.map print_rule(fmtr) rules; fprintf(fmtr, "") // NOOP
          }
        );
      | UHExp.ListNil => print_list_nil(fmtr)
      | UHExp.EmptyHole(_) => fprintf(fmtr, "{}")
      | UHExp.OpSeq(_, seq) =>
        print_opseq(fmtr, print_uhexp, string_of_expop, seq)
      | UHExp.ApPalette(name, serialized_model) =>
        fprintf(fmtr, "%s \"%s\"", name, String.escaped(serialized_model))
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
