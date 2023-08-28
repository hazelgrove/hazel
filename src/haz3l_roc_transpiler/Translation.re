open Haz3lcore;

exception UnsupportedInput;
exception ListConcatError;

let rec get_roc_exp =
        (t: Haz3lcore.TermBase.UExp.t, m: Haz3lcore.Statics.Map.t)
        : TermRoc.UExp.t =>
  switch (t.term) {
  | Invalid(_) => raise(UnsupportedInput)
  | EmptyHole => raise(UnsupportedInput)
  | MultiHole(_) => raise(UnsupportedInput)
  | Constructor(_) => raise(UnsupportedInput)
  | TyAlias(_) => raise(UnsupportedInput)
  | ListConcat(t1, t2) => ListLit(get_roc_concat_list(t1, t2, m))
  | Triv => Record([])
  | Bool(b) => Bool(b)
  | Int(n) => Int(n)
  | Float(f) => Float(f)
  | String(s) => String(s)
  | ListLit(l) => ListLit(get_roc_exp_list(l, m))
  | Fun(p, t) => Fun(get_roc_pat(p, m), get_roc_exp(t, m))
  | Tuple(l) => Record(get_roc_exp_list(l, m))
  | Var(token) => Var(get_name(m, token))
  | Let(pat, def, body) =>
    let indent_flag =
      switch (def.term) {
      | Let(_) => true
      | Invalid(_)
      | EmptyHole
      | MultiHole(_)
      | Constructor(_)
      | TyAlias(_)
      | ListConcat(_)
      | Triv
      | Bool(_)
      | Int(_)
      | Float(_)
      | String(_)
      | ListLit(_)
      | Fun(_)
      | Tuple(_)
      | Var(_)
      | Ap(_)
      | If(_)
      | Seq(_)
      | Test(_)
      | Parens(_)
      | Cons(_)
      | UnOp(_)
      | BinOp(_)
      | Match(_) => false
      };
    switch (get_typ_ann(pat, m)) {
    // If the let expression contains another let expression in its definition,
    // then this new let expression should be indented to indicate its scope
    | Some(p) =>
      if (indent_flag) {
        Seq([
          p,
          SeqLetIndent(
            Assign(get_pat_var(pat, m), get_roc_exp(def, m)),
            get_roc_exp(body, m),
          ),
        ]);
      } else {
        Seq([
          p,
          Assign(get_pat_var(pat, m), get_roc_exp(def, m)),
          get_roc_exp(body, m),
        ]);
      }
    | None =>
      if (indent_flag) {
        SeqLetIndent(
          Assign(get_pat_var(pat, m), get_roc_exp(def, m)),
          get_roc_exp(body, m),
        );
      } else {
        Seq([
          Assign(get_pat_var(pat, m), get_roc_exp(def, m)),
          get_roc_exp(body, m),
        ]);
      }
    };
  | Ap(fn, v) => Ap(get_roc_exp(fn, m), get_roc_exp(v, m))
  | If(cond, if_true, if_false) =>
    If(
      get_roc_exp(cond, m),
      get_roc_exp(if_true, m),
      get_roc_exp(if_false, m),
    )
  | Seq(_, t2) => get_roc_exp(t2, m)
  | Test(t) => Expect(get_roc_exp(t, m))
  | Parens(t) =>
    switch (t.term) {
    | Tuple(_) => get_roc_exp(t, m)
    | _ => Parens(get_roc_exp(t, m))
    }
  | Cons(hd, tl) =>
    switch (get_roc_exp(tl, m)) {
    | ListLit([]) => ListLit([get_roc_exp(hd, m)])
    | ListLit(l) => ListLit(List.append([get_roc_exp(hd, m)], l))
    | _ =>
      SeqNoBreak([
        Var("List.concat"),
        ListLit([get_roc_exp(hd, m)]),
        Parens(get_roc_exp(tl, m)),
      ])
    }
  | UnOp(op_un, v) => UnOp(get_roc_op_un(op_un), get_roc_exp(v, m))
  | BinOp(op_bin, v1, v2) =>
    BinOp(get_roc_op_bin(op_bin), get_roc_exp(v1, m), get_roc_exp(v2, m))
  | Match(t, l) =>
    let scrut = get_roc_exp(t, m);
    Match(scrut, get_roc_exp_match(l, scrut, m));
  }
and get_roc_exp_list =
    (list: list(Haz3lcore.TermBase.UExp.t), m: Haz3lcore.Statics.Map.t) =>
  switch (list) {
  | [] => []
  | [x, ...xs] => [get_roc_exp(x, m), ...get_roc_exp_list(xs, m)]
  }
and get_roc_concat_list =
    (
      t1: Haz3lcore.TermBase.UExp.t,
      t2: Haz3lcore.TermBase.UExp.t,
      m: Haz3lcore.Statics.Map.t,
    ) =>
  switch (t1.term, t2.term) {
  | (ListLit(l1), ListLit(l2)) => get_roc_exp_list(List.concat([l1, l2]), m)
  | (ListLit(l1), ListConcat(t2, t3)) =>
    List.concat([get_roc_exp_list(l1, m), get_roc_concat_list(t2, t3, m)])
  | (ListConcat(t1, t2), ListLit(l3)) =>
    List.concat([get_roc_concat_list(t1, t2, m), get_roc_exp_list(l3, m)])
  | (ListConcat(t1, t2), ListConcat(t3, t4)) =>
    List.concat([
      get_roc_concat_list(t1, t2, m),
      get_roc_concat_list(t3, t4, m),
    ])
  | _ => raise(ListConcatError)
  }

and get_name = (map: Haz3lcore.Statics.Map.t, name: string) =>
  if (!String.contains(name, '_')) {
    name;
  } else {
    let count = ref(0);
    let new_name = ref(get_camel_case(name));
    let tmp_name = ref(name ++ string_of_int(count^));
    while (iterate_map(map, new_name, tmp_name)) {
      new_name := get_camel_case(name) ++ string_of_int(count^);
      tmp_name := name ++ string_of_int(count^);
      count := count^ + 1;
    };
    new_name^;
  }
and get_camel_case = (name: string) => {
  let parts = String.split_on_char('_', name);
  let capitalizedParts = List.map(String.capitalize_ascii, List.tl(parts));
  List.hd(parts) ++ String.concat("", capitalizedParts);
}
and iterate_map =
    (map: Haz3lcore.Statics.Map.t, name: ref(string), tmp: ref(string)) => {
  let flag = ref(false);
  Id.Map.iter(
    (_, value) => {
      switch (value) {
      | Info.InfoExp(infoExp) =>
        switch (infoExp.term.term) {
        | Var(s) =>
          if (String.equal(s, name^) || String.equal(s, tmp^)) {
            flag := true;
          }
        | _ => ()
        }
      | Info.InfoPat(infoPat) =>
        switch (infoPat.term.term) {
        | Var(s) =>
          if (String.equal(s, name^) || String.equal(s, tmp^)) {
            flag := true;
          }

        | _ => ()
        }
      | _ => ()
      }
    },
    map,
  );
  flag^;
}
and get_pat_var_list =
    (list: list(TermBase.UPat.t), m: Haz3lcore.Statics.Map.t) =>
  switch (list) {
  | [] => []
  | [x, ...xs] => [get_pat_var(x, m), ...get_pat_var_list(xs, m)]
  }
and get_pat_var = (pat: TermBase.UPat.t, m: Haz3lcore.Statics.Map.t) =>
  switch (pat.term) {
  | TypeAnn(p, _) => get_pat_var(p, m)
  | Parens(p) =>
    switch (p.term) {
    | Tuple(_) => get_pat_var(p, m)
    | _ => Parens(get_pat_var(p, m))
    }
  | Var(p) => Var(get_name(m, p))
  | Tuple(l) => Record(get_pat_var_list(l, m))
  | ListLit(_) => String("ListLit Not Implemented")
  | Cons(_) => String("Cons Not Implemented")
  | Invalid(_)
  | EmptyHole
  | MultiHole(_)
  | Wild
  | Int(_)
  | Float(_)
  | Bool(_)
  | String(_)
  | Triv
  | Constructor(_)
  | Ap(_) => String("Invalid")
  }
and get_typ_ann = (pat: TermBase.UPat.t, m: Haz3lcore.Statics.Map.t) =>
  switch (pat.term) {
  | Parens(pat) => get_typ_ann(pat, m)
  | TypeAnn(v, typ) => Some(TypeAnn(get_var(v, m), get_roc_typ(typ, m)))
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
  | Constructor(_)
  | Cons(_)
  | Tuple(_)
  | Ap(_) => None
  }
and get_var = (pat: TermBase.UPat.t, m: Haz3lcore.Statics.Map.t): string =>
  switch (pat.term) {
  | Parens(pat) => get_var(pat, m)
  | Var(x) => get_name(m, x)
  | TypeAnn(p, _) => get_var(p, m)
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
  | Constructor(_)
  | Cons(_)
  | Tuple(_)
  | Ap(_) => "None"
  }
and get_roc_op_un = (op_un_hazel: TermBase.UExp.op_un): TermRoc.UExp.op_un =>
  switch (op_un_hazel) {
  | Int(Minus) => Int(Minus)
  | Bool(Not) => Bool(Not)
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
    | NotEquals => Int_Float(NotEquals)
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
    | NotEquals => Int_Float(NotEquals)
    }
  | Bool(op_bin_bool) =>
    switch (op_bin_bool) {
    | And => Bool(And)
    | Or => Bool(Or)
    }
  | String(op_bin_string) =>
    switch (op_bin_string) {
    | Equals => String(Equals)
    | Concat => raise(UnsupportedInput)
    }
  }
and get_roc_exp_match =
    (
      list: list((TermBase.UPat.t, TermBase.UExp.t)),
      scrut: TermRoc.UExp.t,
      m: Haz3lcore.Statics.Map.t,
    ) =>
  switch (list) {
  | [] => []
  | [(p, t), ...xs] =>
    switch (p.term) {
    | Cons(_, tl) =>
      switch (get_roc_exp_tl(tl, 1)) {
      | (false, _, _) => [
          (get_roc_pat(p, m), get_roc_exp(t, m)),
          ...get_roc_exp_match(xs, scrut, m),
        ]
      | (true, var, count) =>
        let seqlist: ref(list(TermRoc.UExp.t)) = ref([]);
        if (count > 1) {
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
        } else {
          seqlist :=
            List.append(
              seqlist^,
              [
                Assign(
                  Var(var),
                  SeqNoBreak([Var("List.dropAt"), scrut, Var("0")]),
                ),
              ],
            );
        };
        [
          (
            get_roc_pat(p, m),
            SeqMatchIndent(Seq(seqlist^), get_roc_exp(t, m)),
          ),
          ...get_roc_exp_match(xs, scrut, m),
        ];
      }
    | _ => [
        (get_roc_pat(p, m), get_roc_exp(t, m)),
        ...get_roc_exp_match(xs, scrut, m),
      ]
    }
  }
and get_roc_exp_tl = (tl: TermBase.UPat.t, c: int): (bool, string, int) => {
  switch (tl.term) {
  | Cons(_, tl) => get_roc_exp_tl(tl, c + 1)
  | Var(t) => (true, t, c)
  | _ => (false, "", 0)
  };
}
and get_roc_pat =
    (t: TermBase.UPat.t, m: Haz3lcore.Statics.Map.t): TermRoc.UPat.t =>
  switch (t.term) {
  | Invalid(_) => raise(UnsupportedInput)
  | EmptyHole => raise(UnsupportedInput)
  | MultiHole(_) => raise(UnsupportedInput)
  | Constructor(_) => raise(UnsupportedInput)
  | Ap(_) => raise(UnsupportedInput)
  | Triv => Record([])
  | Wild => Wild
  | Int(n) => Int(n)
  | Float(f) => Float(f)
  | Bool(b) => Bool(b)
  | String(s) => String(s)
  | ListLit(l) => ListLit(get_roc_pat_list(l, m))
  | Cons(hd, tl) => get_roc_pat_cons(hd, tl, m)
  | Var(t) => Var(get_name(m, t))
  | Tuple(l) => Record(get_roc_pat_list(l, m))
  | Parens(t) =>
    switch (t.term) {
    | Tuple(_) => get_roc_pat(t, m)
    | _ => Parens(get_roc_pat(t, m))
    }
  | TypeAnn(v, _) => get_roc_pat(v, m)
  }
and get_roc_pat_list =
    (list: list(TermBase.UPat.t), m: Haz3lcore.Statics.Map.t) =>
  switch (list) {
  | [] => []
  | [x, ...xs] => [get_roc_pat(x, m), ...get_roc_pat_list(xs, m)]
  }
and get_roc_pat_cons =
    (hd: TermBase.UPat.t, tl: TermBase.UPat.t, m: Haz3lcore.Statics.Map.t)
    : TermRoc.UPat.t => {
  switch (get_roc_pat(tl, m)) {
  | ListLit([]) => ListLit([get_roc_pat(hd, m)])
  | ListLit(l) => ListLit(List.append([get_roc_pat(hd, m)], l))
  | Var(_) => ListLit(List.append([get_roc_pat(hd, m)], [Rest]))
  | Wild => ListLit(List.append([get_roc_pat(hd, m)], [Rest]))
  | _ => Bool(false)
  };
}
and get_roc_typ =
    (t: TermBase.UTyp.t, m: Haz3lcore.Statics.Map.t): TermRoc.UTyp.t =>
  switch (t.term) {
  | Invalid(_) => raise(UnsupportedInput)
  | EmptyHole => raise(UnsupportedInput)
  | MultiHole(_) => raise(UnsupportedInput)
  | Constructor(_) => raise(UnsupportedInput)
  | Sum(_) => raise(UnsupportedInput)
  | Ap(_) => raise(UnsupportedInput)
  | Int => Int
  | Float => Float
  | Bool => Bool
  | String => String
  | List(t2) => List(get_roc_typ(t2, m))
  | Var(s) => Var(get_name(m, s))
  | Arrow(t1, t2) => Arrow(get_roc_typ(t1, m), get_roc_typ(t2, m))
  | Tuple(l) => Record(get_roc_typ_list(l, m))
  | Parens(t2) =>
    switch (t2.term) {
    | Tuple(_) => get_roc_typ(t2, m)
    | _ => Parens(get_roc_typ(t2, m))
    }
  }
and get_roc_typ_list =
    (list: list(TermBase.UTyp.t), m: Haz3lcore.Statics.Map.t) =>
  switch (list) {
  | [] => []
  | [x, ...xs] => [get_roc_typ(x, m), ...get_roc_typ_list(xs, m)]
  };
