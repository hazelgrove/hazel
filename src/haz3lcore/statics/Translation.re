let rec get_roc_term = (t: TermBase.UExp.t): TermRoc.UExp.t =>
  switch (t.term) {
  // | Invalid(parse_flag)
  // | EmptyHole
  // | MultiHole(list(Any.t))
  // | Tag(s) => Tag(s)
  | Triv => Record([])
  | Bool(b) => Bool(b)
  | Int(n) => Int(n)
  | Float(f) => Float(f)
  | String(s) => String(s)
  | ListLit(l) => ListLit(List.map(get_roc_term, l))
  | Fun(p, t) =>
    // let ind =
    //   switch (t.term) {
    //   | Let(_) => i + 1
    //   | _ => i
    //   };
    Fun(get_roc_pat_term(p), get_roc_term(t))
  | Tuple(l) => Record(List.map(get_roc_term, l))
  // | Tuple(l) => Tuple(List.map(get_roc_term, l))
  | Var(token) => Var(get_camel_case(token))
  | Let(pat, def, body) =>
    let indent_flag =
      switch (def.term) {
      | Let(_) => true
      | _ => false
      };
    switch (get_typ_ann(pat)) {
    | Some(p) =>
      if (indent_flag) {
        Seq(
          p,
          SeqIndent(
            Assign(get_pat_var(pat), get_roc_term(def)),
            get_roc_term(body),
          ),
        );
      } else {
        Seq(
          p,
          Seq(
            Assign(get_pat_var(pat), get_roc_term(def)),
            get_roc_term(body),
          ),
        );
      }
    | None =>
      if (indent_flag) {
        SeqIndent(
          Assign(get_pat_var(pat), get_roc_term(def)),
          get_roc_term(body),
        );
      } else {
        Seq(
          Assign(get_pat_var(pat), get_roc_term(def)),
          get_roc_term(body),
        );
      }
    };
  | Ap(fn, v) => Ap(get_roc_term(fn), get_roc_term(v))
  | If(cond, if_true, if_false) =>
    If(get_roc_term(cond), get_roc_term(if_true), get_roc_term(if_false))
  // | Seq(t1, t2) => Seq(get_roc_term(t1, i), get_roc_term(t2, i))
  | Test(t) => Expect(get_roc_term(t))
  | Parens(t) =>
    switch (t.term) {
    | Tuple(_) => get_roc_term(t)
    | _ => Parens(get_roc_term(t))
    }
  // | Parens(t) => Parens(get_roc_term(t))
  | Cons(hd, tl) =>
    switch (get_roc_term(tl)) {
    | ListLit([]) => ListLit([get_roc_term(hd)])
    | ListLit(l) => ListLit(List.append([get_roc_term(hd)], l))
    | _ => Bool(false)
    }
  | UnOp(op_un, v) => UnOp(get_roc_op_un(op_un), get_roc_term(v))
  | BinOp(op_bin, v1, v2) =>
    BinOp(get_roc_op_bin(op_bin), get_roc_term(v1), get_roc_term(v2))
  | Match(t, l) =>
    let scrut = get_roc_term(t);
    Match(scrut, get_roc_list_match(l, scrut));
  | _ => String("Not implemented")
  }

and get_roc_list = (list: list(TermBase.UExp.t)) =>
  switch (list) {
  | [] => []
  | [x, ...xs] => [get_roc_term(x), ...get_roc_list(xs)]
  }
and get_roc_list_match =
    (list: list((TermBase.UPat.t, TermBase.UExp.t)), scrut: TermRoc.UExp.t) =>
  switch (list) {
  | [] => []
  // | [(p, t), ...xs] => [
  //     (get_roc_pat_term(p), get_roc_term(t)),
  //     ...get_roc_list_match(xs),
  //   ]
  | [(p, t), ...xs] =>
    switch (p.term) {
    | Cons(_, tl) =>
      switch (get_cons_tl(tl, 1)) {
      | (false, _, _) => [
          (get_roc_pat_term(p), get_roc_term(t)),
          ...get_roc_list_match(xs, scrut),
        ]
      | (true, var, count) =>
        let seqlist: ref(list(TermRoc.UExp.t)) = ref([]);
        seqlist :=
          List.append(
            seqlist^,
            [
              Assign(
                Var(var ++ "0"),
                SeqNoBreak([Var("List.dropAt"), scrut, Var("0")]),
              ),
            ],
          );
        for (i in 1 to count - 2) {
          seqlist :=
            List.append(
              seqlist^,
              [
                Assign(
                  Var(var ++ string_of_int(i)),
                  SeqNoBreak([
                    Var("List.dropAt"),
                    Var(var ++ string_of_int(i - 1)),
                    Var(string_of_int(i)),
                  ]),
                ),
              ],
            );
        };
        seqlist :=
          List.append(
            seqlist^,
            [
              Assign(
                Var(var),
                SeqNoBreak([
                  Var("List.dropAt"),
                  Var(var ++ string_of_int(count - 2)),
                  Var(string_of_int(count - 1)),
                ]),
              ),
            ],
          );
        [
          (
            get_roc_pat_term(p),
            SeqMatchIndent(SeqList(seqlist^), get_roc_term(t)),
          ),
          ...get_roc_list_match(xs, scrut),
        ];
      }
    | _ => [
        (get_roc_pat_term(p), get_roc_term(t)),
        ...get_roc_list_match(xs, scrut),
      ]
    }
  }

and get_cons_tl = (tl: TermBase.UPat.t, c: int): (bool, string, int) => {
  switch (tl.term) {
  | Cons(_, tl) => get_cons_tl(tl, c + 1)
  | Var(t) => (true, t, c)
  | _ => (false, "", 0)
  };
}

and get_roc_pat_term = (t: TermBase.UPat.t): TermRoc.UPat.t =>
  switch (t.term) {
  // | Invalid(parse_flag) => String("Triv") //
  // | EmptyHole => String("Triv") //
  // | MultiHole(list(Any.t)) => String("Triv") //
  // | Tag(s) => Tag(s) //
  // | Ap(fn, v) => Ap(get_roc_pat_term(fn), get_roc_pat_term(v))
  | Triv => Record([])
  | Wild => Wild
  | Int(n) => Int(n)
  | Float(f) => Float(f)
  | Bool(b) => Bool(b)
  | String(s) => String(s)
  | ListLit(l) => ListLit(List.map(get_roc_pat_term, l))
  | Cons(hd, tl) => get_cons_list(hd, tl)
  | Var(t) => Var(get_camel_case(t))
  | Tuple(l) => Record(List.map(get_roc_pat_term, l))
  // | Tuple(l) => Tuple(List.map(get_roc_pat_term, l))
  | Parens(t) =>
    switch (t.term) {
    | Tuple(_) => get_roc_pat_term(t)
    | _ => Parens(get_roc_pat_term(t))
    }
  // | Parens(t) => Parens(get_roc_pat_term(t))
  | TypeAnn(v, _) => get_roc_pat_term(v)
  | _ => String("Not implemented")
  }

and get_cons_list = (hd: TermBase.UPat.t, tl: TermBase.UPat.t): TermRoc.UPat.t => {
  switch (get_roc_pat_term(tl)) {
  | ListLit([]) => ListLit([get_roc_pat_term(hd)])
  | ListLit(l) => ListLit(List.append([get_roc_pat_term(hd)], l))
  | Var(_) => ListLit(List.append([get_roc_pat_term(hd)], [Rest]))
  | Wild => ListLit(List.append([get_roc_pat_term(hd)], [Rest]))
  | _ => Bool(false)
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
and get_pat_var = (pat: TermBase.UPat.t) =>
  switch (pat.term) {
  | TypeAnn(p, _) => get_pat_var(p)
  | Parens(p) =>
    switch (p.term) {
    | Tuple(_) => get_pat_var(p)
    | _ => Parens(get_pat_var(p))
    }
  | Var(p) => Var(get_camel_case(p))
  // | Tuple(l) => Tuple(List.map(get_pat_var, l))
  | Tuple(l) => Record(List.map(get_pat_var, l))
  | ListLit(_) => String("Need to be implemented")
  | Cons(_) => String("Need to be implemented")
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
  | Ap(_) => String("Invalid")
  }
and get_typ_ann = (pat: TermBase.UPat.t) =>
  switch (pat.term) {
  | Parens(pat) => get_typ_ann(pat)
  | TypeAnn(v, typ) => Some(TypeAnn(get_var(v), get_roc_type(typ)))
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
