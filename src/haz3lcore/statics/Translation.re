let rec get_roc_term = (t: TermBase.UExp.t, i: int): TermRoc.UExp.t =>
  switch (t.term) {
  // | Invalid(parse_flag)
  // | EmptyHole
  // | MultiHole(list(Any.t))
  // | Tag(s) => Tag(s)
  | Triv => {term: Record([]), indent: i}
  | Bool(b) => {term: Bool(b), indent: i}
  | Int(n) => {term: Int(n), indent: i}
  | Float(f) => {term: Float(f), indent: i}
  | String(s) => {term: String(s), indent: i}
  | ListLit(l) => {term: ListLit(get_roc_list(l, i)), indent: i}
  | Fun(p, t) =>
    let ind =
      switch (t.term) {
      | Let(_) => i + 1
      | _ => i
      };
    {term: Fun(get_roc_pat_term(p, i), get_roc_term(t, ind)), indent: ind};
  | Tuple(l) => {term: Record(get_roc_list(l, i)), indent: i}
  // | Tuple(l) => Tuple(List.map(get_roc_term, l))
  | Var(token) => {term: Var(get_camel_case(token)), indent: i}
  | Let(pat, def, body) =>
    let ind =
      switch (def.term) {
      | Let(_) => i + 1
      | _ => i
      };
    switch (get_typ_ann(pat, i)) {
    | Some(p) => {
        term:
          Seq(
            p,
            {
              term:
                Seq(
                  {
                    term:
                      Assign(get_pat_var(pat, i), get_roc_term(def, ind)),
                    indent: ind,
                  },
                  get_roc_term(body, i),
                ),
              indent: i,
            },
          ),
        indent: i,
      }
    | None => {
        term:
          Seq(
            {
              term: Assign(get_pat_var(pat, i), get_roc_term(def, ind)),
              indent: ind,
            },
            get_roc_term(body, i),
          ),
        indent: i,
      }
    };
  | Ap(fn, v) => {
      term: Ap(get_roc_term(fn, i), get_roc_term(v, i)),
      indent: i,
    }
  | If(cond, if_true, if_false) => {
      term:
        If(
          get_roc_term(cond, i),
          get_roc_term(if_true, i),
          get_roc_term(if_false, i),
        ),
      indent: i,
    }
  | Seq(t1, t2) => {
      term: Seq(get_roc_term(t1, i), get_roc_term(t2, i)),
      indent: i,
    }
  | Test(t) => {term: Expect(get_roc_term(t, i)), indent: i}
  | Parens(t) =>
    switch (t.term) {
    | Tuple(_) => get_roc_term(t, i)
    | _ => {term: Parens(get_roc_term(t, i)), indent: i}
    }
  // | Parens(t) => Parens(get_roc_term(t))
  | Cons(hd, tl) =>
    switch (get_roc_term(tl, i).term) {
    | ListLit([]) => {term: ListLit([get_roc_term(hd, i)]), indent: i}
    | ListLit(l) => {
        term: ListLit(List.append([get_roc_term(hd, i)], l)),
        indent: i,
      }
    | _ => {term: Bool(false), indent: i}
    }
  | UnOp(op_un, v) => {
      term: UnOp(get_roc_op_un(op_un), get_roc_term(v, i)),
      indent: i,
    }
  | BinOp(op_bin, v1, v2) => {
      term:
        BinOp(
          get_roc_op_bin(op_bin),
          get_roc_term(v1, i),
          get_roc_term(v2, i),
        ),
      indent: i,
    }
  | Match(t, l) => {
      term: Match(get_roc_term(t, i), get_roc_list_match(l, i)),
      indent: i,
    }
  | _ => {term: String("Not implemented"), indent: i}
  }

and get_roc_list = (list: list(TermBase.UExp.t), i: int) =>
  switch (list) {
  | [] => []
  | [x, ...xs] => [get_roc_term(x, i), ...get_roc_list(xs, i)]
  }
and get_roc_list_pat = (list: list(TermBase.UPat.t), i: int, func) =>
  switch (list) {
  | [] => []
  | [x, ...xs] => [func(x, i), ...get_roc_list_pat(xs, i, func)]
  }
and get_roc_list_match =
    (list: list((TermBase.UPat.t, TermBase.UExp.t)), i: int) =>
  switch (list) {
  | [] => []
  | [(p, t), ...xs] => [
      (get_roc_pat_term(p, i), get_roc_term(t, i)),
      ...get_roc_list_match(xs, i),
    ]
  }

and get_roc_pat_term = (t: TermBase.UPat.t, i: int): TermRoc.UPat.t =>
  switch (t.term) {
  // | Invalid(parse_flag) => String("Triv") //
  // | EmptyHole => String("Triv") //
  // | MultiHole(list(Any.t)) => String("Triv") //
  // | Tag(s) => Tag(s) //
  // | Ap(fn, v) => Ap(get_roc_pat_term(fn), get_roc_pat_term(v))
  | Triv => {term: Record([]), indent: i}
  | Wild => {term: Wild, indent: i}
  | Int(n) => {term: Int(n), indent: i}
  | Float(f) => {term: Float(f), indent: i}
  | Bool(b) => {term: Bool(b), indent: i}
  | String(s) => {term: String(s), indent: i}
  | ListLit(l) => {
      term: ListLit(get_roc_list_pat(l, i, get_roc_pat_term)),
      indent: i,
    }
  | Cons(hd, tl) => get_cons_list(hd, tl, i)
  | Var(t) => {term: Var(get_camel_case(t)), indent: i}
  | Tuple(l) => {
      term: Record(get_roc_list_pat(l, i, get_roc_pat_term)),
      indent: i,
    }
  // | Tuple(l) => Tuple(List.map(get_roc_pat_term, l))
  | Parens(t) =>
    switch (t.term) {
    | Tuple(_) => get_roc_pat_term(t, i)
    | _ => {term: Parens(get_roc_pat_term(t, i)), indent: i}
    }
  // | Parens(t) => Parens(get_roc_pat_term(t))
  | TypeAnn(v, _) => get_roc_pat_term(v, i)
  | _ => {term: String("Not implemented"), indent: i}
  }

and get_cons_list =
    (hd: TermBase.UPat.t, tl: TermBase.UPat.t, i: int): TermRoc.UPat.t => {
  switch (get_roc_pat_term(tl, i).term) {
  | ListLit([]) => {term: ListLit([get_roc_pat_term(hd, i)]), indent: i}
  | ListLit(l) => {
      term: ListLit(List.append([get_roc_pat_term(hd, i)], l)),
      indent: i,
    }
  | Var(_) => {
      term:
        ListLit(
          List.append(
            [get_roc_pat_term(hd, i)],
            [{term: Rest, indent: i}],
          ),
        ),
      indent: i,
    }
  | Wild => {
      term:
        ListLit(
          List.append(
            [get_roc_pat_term(hd, i)],
            [{term: Rest, indent: i}],
          ),
        ),
      indent: i,
    }
  | _ => {term: Bool(false), indent: i}
  };
}

and get_roc_type = (t: TermBase.UTyp.t): TermRoc.UTyp.t =>
  switch (t.term) {
  // | Invalid(parse_flag)
  // | EmptyHole
  // | MultiHole(list(Any.t))
  | Int => Int
  | Float => Float
  | Bool => Bool
  | String => String
  | List(t) => List(get_roc_type(t))
  | Var(s) => Var(get_camel_case(s))
  | Arrow(t1, t2) => Arrow(get_roc_type(t1), get_roc_type(t2))
  | Tuple(l) => Record(get_roc_list_type(l))
  // | Tuple(l) => Tuple(List.map(get_roc_type, l))
  | Parens(t) => Parens(get_roc_type(t))
  | _ => Var("Not_implemented")
  }
and get_roc_list_type = (list: list(TermBase.UTyp.t)) =>
  switch (list) {
  | [] => []
  | [x, ...xs] => [get_roc_type(x), ...get_roc_list_type(xs)]
  }
and get_roc_op_un = (op_un_hazel: TermBase.UExp.op_un): TermRoc.UExp.op_un =>
  switch (op_un_hazel) {
  | Int(Minus) => Int(Minus)
  }
and get_roc_op_bin = (op_bin_hazel: TermBase.UExp.op_bin): TermRoc.UExp.op_bin =>
  switch (op_bin_hazel) {
  | Int(op_bin_int) =>
    switch (op_bin_int) {
    | Plus => Int_Float(Plus)
    | Minus => Int_Float(Minus)
    | Times => Int_Float(Times)
    | Power => Int_Float(Power)
    | Divide => Int_Float(Divide)
    | LessThan => Int_Float(LessThan)
    | LessThanOrEqual => Int_Float(LessThanOrEqual)
    | GreaterThan => Int_Float(GreaterThan)
    | GreaterThanOrEqual => Int_Float(GreaterThanOrEqual)
    | Equals => Int_Float(Equals)
    }
  | Float(op_bin_float) =>
    switch (op_bin_float) {
    | Plus => Int_Float(Plus)
    | Minus => Int_Float(Minus)
    | Times => Int_Float(Times)
    | Power => Int_Float(Power)
    | Divide => Int_Float(Divide)
    | LessThan => Int_Float(LessThan)
    | LessThanOrEqual => Int_Float(LessThanOrEqual)
    | GreaterThan => Int_Float(GreaterThan)
    | GreaterThanOrEqual => Int_Float(GreaterThanOrEqual)
    | Equals => Int_Float(Equals)
    }
  | Bool(op_bin_bool) =>
    switch (op_bin_bool) {
    | And => Bool(And)
    | Or => Bool(Or)
    }
  | String(op_bin_string) =>
    switch (op_bin_string) {
    | Equals => String(Equals)
    }
  }
and get_var = (pat: TermBase.UPat.t): string =>
  switch (pat.term) {
  | Parens(pat) => get_var(pat)
  | Var(x) => get_camel_case(x)
  | TypeAnn(p, _) => get_var(p)
  | Invalid(_)
  | EmptyHole
  | MultiHole(_)
  | Wild
  | Int(_)
  | Float(_)
  | Bool(_)
  | String(_)
  | Triv
  | ListLit(_)
  | Tag(_)
  | Cons(_)
  | Tuple(_)
  | Ap(_) => "None"
  }
and get_pat_var = (pat: TermBase.UPat.t, i: int) =>
  switch (pat.term) {
  | TypeAnn(p, _) => get_pat_var(p, i)
  | Parens(p) =>
    switch (p.term) {
    | Tuple(_) => get_pat_var(p, i)
    | _ => {term: Parens(get_pat_var(p, i)), indent: i}
    }
  | Var(p) => {term: Var(get_camel_case(p)), indent: i}
  // | Tuple(l) => Tuple(List.map(get_pat_var, l)), indent: i}
  | Tuple(l) => {
      term: Record(get_roc_list_pat(l, i, get_pat_var)),
      indent: i,
    }
  | ListLit(_) => {term: String("Need to be implemented"), indent: i}
  | Cons(_) => {term: String("Need to be implemented"), indent: i}
  | Invalid(_)
  | EmptyHole
  | MultiHole(_)
  | Wild
  | Int(_)
  | Float(_)
  | Bool(_)
  | String(_)
  | Triv
  | Tag(_)
  | Ap(_) => {term: String("Invalid"), indent: i}
  }
and get_typ_ann = (pat: TermBase.UPat.t, i: int) =>
  switch (pat.term) {
  | Parens(pat) => get_typ_ann(pat, i)
  | TypeAnn(v, typ) =>
    Some({term: TypeAnn(get_var(v), get_roc_type(typ)), indent: i})
  | Var(_)
  | Invalid(_)
  | EmptyHole
  | MultiHole(_)
  | Wild
  | Int(_)
  | Float(_)
  | Bool(_)
  | String(_)
  | Triv
  | ListLit(_)
  | Tag(_)
  | Cons(_)
  | Tuple(_)
  | Ap(_) => None
  }

and get_camel_case = (name: string) => {
  let parts = String.split_on_char('_', name);
  let capitalizedParts = List.map(String.capitalize_ascii, List.tl(parts));
  List.hd(parts) ++ String.concat("", capitalizedParts);
};
