type t = Frame_base.t(Term_exp.tile, ftile)
and ftile =
  | Root
  | Paren_body(t)
  | Case_scrut(Term_exp.rules, t)
  | Case_clause(Term_exp.t, Term_pat.s, ListFrame.t(Term_exp.rule), t)
  | Let_def(Term_pat.s, t)
  | Ap_arg(t);
