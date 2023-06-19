let rec get_roc_term = (t: TermBase.UExp.t, m: Statics.map): TermRoc.UExp.t =>
  switch (t.term) {
  | Invalid(_) => String("Invalid Not implemented")
  | EmptyHole => String("EmptyHole Not implemented")
  | MultiHole(_) => String("MultiHole Not implemented")
  | Tag(_) => String("Tag Not implemented")
  | Triv => Record([])
  | Bool(b) => Bool(b)
  | Int(n) => Int(n)
  | Float(f) => Float(f)
  | String(s) => String(s)
  // | ListLit(l) => ListLit(List.map(get_roc_term, l))
  | ListLit(l) => ListLit(get_roc_list(l, m))
  | Fun(p, t) => Fun(get_roc_pat_term(p, m), get_roc_term(t, m))
  // | Tuple(l) => Record(List.map(get_roc_term, l))
  | Tuple(l) => Record(get_roc_list(l, m))
  | Var(token) => Var(get_name(m, token))
  | Let(pat, def, body) =>
    let indent_flag =
      switch (def.term) {
      | Let(_) => true
      | _ => false
      };
    switch (get_typ_ann(pat, m)) {
    | Some(p) =>
      if (indent_flag) {
        Seq([
          p,
          SeqIndent(
            Assign(get_pat_var(pat, m), get_roc_term(def, m)),
            get_roc_term(body, m),
          ),
        ]);
      } else {
        Seq([
          p,
          Assign(get_pat_var(pat, m), get_roc_term(def, m)),
          get_roc_term(body, m),
        ]);
      }
    | None =>
      if (indent_flag) {
        SeqIndent(
          Assign(get_pat_var(pat, m), get_roc_term(def, m)),
          get_roc_term(body, m),
        );
      } else {
        Seq([
          Assign(get_pat_var(pat, m), get_roc_term(def, m)),
          get_roc_term(body, m),
        ]);
      }
    };
  | Ap(fn, v) => Ap(get_roc_term(fn, m), get_roc_term(v, m))
  | If(cond, if_true, if_false) =>
    If(
      get_roc_term(cond, m),
      get_roc_term(if_true, m),
      get_roc_term(if_false, m),
    )
  | Seq(_, t2) => get_roc_term(t2, m)
  | Test(t) => Expect(get_roc_term(t, m))
  | Parens(t) =>
    switch (t.term) {
    | Tuple(_) => get_roc_term(t, m)
    | _ => Parens(get_roc_term(t, m))
    }
  | Cons(hd, tl) =>
    switch (get_roc_term(tl, m)) {
    | ListLit([]) => ListLit([get_roc_term(hd, m)])
    | ListLit(l) => ListLit(List.append([get_roc_term(hd, m)], l))
    | _ =>
      SeqNoBreak([
        Var("List.concat"),
        ListLit([get_roc_term(hd, m)]),
        Parens(get_roc_term(tl, m)),
      ])
    }
  | UnOp(op_un, v) => UnOp(get_roc_op_un(op_un), get_roc_term(v, m))
  | BinOp(op_bin, v1, v2) =>
    BinOp(get_roc_op_bin(op_bin), get_roc_term(v1, m), get_roc_term(v2, m))
  | Match(t, l) =>
    let scrut = get_roc_term(t, m);
    Match(scrut, get_roc_list_match(l, scrut, m));
  }

and get_roc_list = (list: list(TermBase.UExp.t), m: Statics.map) =>
  switch (list) {
  | [] => []
  | [x, ...xs] => [get_roc_term(x, m), ...get_roc_list(xs, m)]
  }
and get_roc_list_match =
    (
      list: list((TermBase.UPat.t, TermBase.UExp.t)),
      scrut: TermRoc.UExp.t,
      m: Statics.map,
    ) =>
  switch (list) {
  | [] => []
  | [(p, t), ...xs] =>
    switch (p.term) {
    | Cons(_, tl) =>
      switch (get_cons_tl(tl, 1)) {
      | (false, _, _) => [
          (get_roc_pat_term(p, m), get_roc_term(t, m)),
          ...get_roc_list_match(xs, scrut, m),
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
            get_roc_pat_term(p, m),
            SeqMatchIndent(Seq(seqlist^), get_roc_term(t, m)),
          ),
          ...get_roc_list_match(xs, scrut, m),
        ];
      }
    | _ => [
        (get_roc_pat_term(p, m), get_roc_term(t, m)),
        ...get_roc_list_match(xs, scrut, m),
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

and get_roc_pat_term = (t: TermBase.UPat.t, m: Statics.map): TermRoc.UPat.t =>
  switch (t.term) {
  // | Invalid(parse_flag) => String("Not implemented") //
  // | EmptyHole => String("Not implemented") //
  // | MultiHole(list(Any.t)) => String("Not implemented") //
  // | Tag(s) => String("Not implemented") //
  // | Ap(fn, v) => String("Not implemented")
  | Triv => Record([])
  | Wild => Wild
  | Int(n) => Int(n)
  | Float(f) => Float(f)
  | Bool(b) => Bool(b)
  | String(s) => String(s)
  // | ListLit(l) => ListLit(List.map(get_roc_pat_term, l))
  | ListLit(l) => ListLit(get_roc_pat_list(l, m))
  | Cons(hd, tl) => get_cons_list(hd, tl, m)
  | Var(t) => Var(get_name(m, t))
  // | Tuple(l) => Record(List.map(get_roc_pat_term, l))
  | Tuple(l) => Record(get_roc_pat_list(l, m))
  | Parens(t) =>
    switch (t.term) {
    | Tuple(_) => get_roc_pat_term(t, m)
    | _ => Parens(get_roc_pat_term(t, m))
    }
  | TypeAnn(v, _) => get_roc_pat_term(v, m)
  | _ => String("Not implemented")
  }

and get_roc_pat_list = (list: list(TermBase.UPat.t), m: Statics.map) =>
  switch (list) {
  | [] => []
  | [x, ...xs] => [get_roc_pat_term(x, m), ...get_roc_pat_list(xs, m)]
  }

and get_cons_list =
    (hd: TermBase.UPat.t, tl: TermBase.UPat.t, m: Statics.map): TermRoc.UPat.t => {
  switch (get_roc_pat_term(tl, m)) {
  | ListLit([]) => ListLit([get_roc_pat_term(hd, m)])
  | ListLit(l) => ListLit(List.append([get_roc_pat_term(hd, m)], l))
  | Var(_) => ListLit(List.append([get_roc_pat_term(hd, m)], [Rest]))
  | Wild => ListLit(List.append([get_roc_pat_term(hd, m)], [Rest]))
  | _ => Bool(false)
  };
}

and get_roc_type = (t: TermBase.UTyp.t, m: Statics.map): TermRoc.UTyp.t =>
  switch (t.term) {
  // | Invalid(parse_flag) => Var("Not_implemented")
  // | EmptyHole => Var("Not_implemented")
  // | MultiHole(list(Any.t)) => Var("Not_implemented")
  | Int => Int
  | Float => Float
  | Bool => Bool
  | String => String
  | List(t2) => List(get_roc_type(t2, m))
  | Var(s) => Var(get_name(m, s))
  | Arrow(t1, t2) => Arrow(get_roc_type(t1, m), get_roc_type(t2, m))
  | Tuple(l) => Record(get_roc_list_type(l, m))
  | Parens(t2) =>
    switch (t2.term) {
    | Tuple(_) => get_roc_type(t2, m)
    | _ => Parens(get_roc_type(t2, m))
    }
  | _ => Var("Not_implemented")
  }
and get_roc_list_type = (list: list(TermBase.UTyp.t), m: Statics.map) =>
  switch (list) {
  | [] => []
  | [x, ...xs] => [get_roc_type(x, m), ...get_roc_list_type(xs, m)]
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
and get_var = (pat: TermBase.UPat.t, m: Statics.map): string =>
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
  | Tag(_)
  | Cons(_)
  | Tuple(_)
  | Ap(_) => "None"
  }
and get_pat_var = (pat: TermBase.UPat.t, m: Statics.map) =>
  switch (pat.term) {
  | TypeAnn(p, _) => get_pat_var(p, m)
  | Parens(p) =>
    switch (p.term) {
    | Tuple(_) => get_pat_var(p, m)
    | _ => Parens(get_pat_var(p, m))
    }
  | Var(p) => Var(get_name(m, p))
  // | Tuple(l) => Record(List.map(get_pat_var, l))
  | Tuple(l) => Record(get_pat_var_list(l, m))
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

and get_pat_var_list = (list: list(TermBase.UPat.t), m: Statics.map) =>
  switch (list) {
  | [] => []
  | [x, ...xs] => [get_pat_var(x, m), ...get_pat_var_list(xs, m)]
  }

and get_typ_ann = (pat: TermBase.UPat.t, m: Statics.map) =>
  switch (pat.term) {
  | Parens(pat) => get_typ_ann(pat, m)
  | TypeAnn(v, typ) => Some(TypeAnn(get_var(v, m), get_roc_type(typ, m)))
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
  // Printf.printf("get_camel_case here\n%!");
  let parts = String.split_on_char('_', name);
  let capitalizedParts = List.map(String.capitalize_ascii, List.tl(parts));
  List.hd(parts) ++ String.concat("", capitalizedParts);
}
and get_name = (map: Statics.map, name: string) =>
  // Printf.printf("get name here\n%!");
  if (!String.contains(name, '_')) {
    name;
  } else {
    let count = ref(0);
    let new_name = ref(get_camel_case(name));
    while (iterate_map(map, new_name)) {
      new_name := get_camel_case(name) ++ string_of_int(count^);
      count := count^ + 1;
    };
    new_name^;
  }

and iterate_map = (map: Statics.map, name: ref(string)) => {
  let flag = ref(false);
  Id.Map.iter(
    (_, value) => {
      switch (value) {
      | Statics.InfoExp(infoExp) =>
        switch (infoExp.term.term) {
        | Var(s) =>
          if (String.equal(s, name^)) {
            flag := String.equal(s, name^);
          }
        | _ => ()
        }
      | Statics.InfoPat(infoPat) =>
        switch (infoPat.term.term) {
        | Var(s) =>
          if (String.equal(s, name^)) {
            flag := String.equal(s, name^);
          }

        | _ => ()
        }
      | _ => ()
      }
    },
    map,
  );
  flag^;
};
