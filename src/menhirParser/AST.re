open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, qcheck, eq)]
type filter_action =
  | Pause
  | Debug
  | Hide
  | Eval;

[@deriving (show({with_path: false}), sexp, qcheck, eq)]
type op_bin_float =
  | Plus
  | Minus
  | Times
  | Power
  | Divide
  | LessThan
  | LessThanOrEqual
  | GreaterThan
  | GreaterThanOrEqual
  | Equals
  | NotEquals;

[@deriving (show({with_path: false}), sexp, qcheck, eq)]
type op_bin_bool =
  | And
  | Or;

[@deriving (show({with_path: false}), sexp, qcheck, eq)]
type op_bin_int =
  | Plus
  | Minus
  | Times
  | Power
  | Divide
  | LessThan
  | LessThanOrEqual
  | GreaterThan
  | GreaterThanOrEqual;

[@deriving (show({with_path: false}), sexp, qcheck, eq)]
type op_bin_string =
  | Concat
  | Equals;

[@deriving (show({with_path: false}), sexp, qcheck, eq)]
type op_bin_poly =
  | Equals
  | NotEquals;

[@deriving (show({with_path: false}), sexp, qcheck, eq)]
type bin_op =
  | IntOp(op_bin_int)
  | FloatOp(op_bin_float)
  | StringOp(op_bin_string)
  | BoolOp(op_bin_bool)
  | PolyOp(op_bin_poly);

[@deriving (show({with_path: false}), sexp, qcheck, eq)]
type op_un_meta =
  | Unquote;

[@deriving (show({with_path: false}), sexp, qcheck, eq)]
type op_un_int =
  | Minus;

[@deriving (show({with_path: false}), sexp, qcheck, eq)]
type op_un_bool =
  | Not;

[@deriving (show({with_path: false}), sexp, qcheck, eq)]
type op_un =
  | Meta(op_un_meta)
  | Int(op_un_int)
  | Bool(op_un_bool);

[@deriving (show({with_path: false}), sexp, qcheck, eq)]
type typ_provenance =
  | Internal
  | EmptyHole;

[@deriving (show({with_path: false}), sexp, eq)]
type tpat =
  | InvalidTPat(string)
  | EmptyHoleTPat
  | VarTPat(string);

[@deriving (show({with_path: false}), sexp, eq)]
type typ =
  | IntType
  | SIntType
  | StringType
  | FloatType
  | BoolType
  | NatType
  | SumTyp(sumtype)
  | UnknownType(typ_provenance)
  | TupleType(list(typ))
  | ArrayType(typ)
  | ArrowType(typ, typ)
  | TypVar(string)
  | InvalidTyp(string)
  | PolyType(tpat, typ)
  | RecType(tpat, typ)
  | LabelType(string)
  | TupLabelType(typ, typ)
  | IndicationTyp(typ)
  | ApTyp(typ, typ)
and sumterm =
  | Variant(string, option(typ))
  | BadEntry(typ)
and sumtype = list(sumterm);

[@deriving (show({with_path: false}), sexp, eq)]
type pat =
  | AscPat(pat, typ)
  | EmptyHolePat
  | WildPat
  | AtomPat(Language.Atom.t)
  | VarPat(string)
  | ConstructorPat(string, option(option(typ)))
  | TuplePat(list(pat))
  | ConsPat(pat, pat)
  | ListPat(list(pat))
  | ApPat(pat, pat)
  | InvalidPat(string) // Menhir parser doesn't actually support invalid pats
  | TupLabelPat(pat, pat)
  | LabelPat(string)
  | IndicationPat(pat);

[@deriving (show({with_path: false}), sexp, qcheck, eq)]
type if_consistency =
  | Consistent
  | Inconsistent;

[@deriving (show({with_path: false}), sexp, qcheck, eq)]
type deferral_pos =
  | InAp
  | OutsideAp;

[@deriving (show({with_path: false}), sexp, eq)]
type exp =
  | Atom(Language.Atom.t)
  | Var(string)
  | Constructor(string, option(option(typ)))
  | ListExp(list(exp))
  | TupleExp(list(exp))
  | BinExp(exp, bin_op, exp)
  | UnOp(op_un, exp)
  | Let(pat, exp, exp)
  | Fun(pat, exp, option(string))
  | CaseExp(exp, list((pat, exp)))
  | Label(string)
  | TupLabel(exp, exp)
  | Dot(exp, exp)
  | ApExp(exp, exp)
  | FixF(pat, exp)
  | Asc(exp, typ)
  | EmptyHole
  | Filter(filter_action, exp, exp)
  | BuiltinFun(string)
  | Undefined
  | Seq(exp, exp)
  | Test(exp)
  | HintedTest(exp, exp)
  | Deferral
  | TypFun(tpat, exp)
  | Cons(exp, exp)
  | ListConcat(exp, exp)
  | If(exp, exp, exp)
  | InvalidExp(string)
  | TypAp(exp, typ)
  | DynamicErrorHole(exp, string)
  | TyAlias(tpat, typ, exp)
  | Use(typ, exp)
  | IndicationExp(exp);

/**
 * Generates a random CONSTRUCTOR_IDENT string. Used for CONSTRUCTOR_IDENT in the lexer.
 *
 * @return A QCheck generator for Constructor Identifier.
 *
 * ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
 */
// TODO handle full constructor ident including nums and '
let gen_constructor_ident: (~minimal_idents: bool) => QCheck.Gen.t(string) =
  (~minimal_idents) =>
    QCheck.Gen.(
      if (minimal_idents) {
        oneof([pure("A"), pure("B")]);
      } else {
        let* leading = char_range('A', 'Z');
        let+ tail = string_size(~gen=char_range('a', 'z'), int_range(1, 4));
        let ident = String.make(1, leading) ++ tail;
        if (List.exists(a => a == ident, ["String", "Int", "Float", "Bool"])) {
          "Keyword";
        } else {
          ident;
        };
      }
    );

/**
 * Generates a random IDENT string. Used for IDENT in the lexer.
 *
 * @return A QCheck generator for Identifier.
 *
 * ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
 */
let gen_ident: (~minimal_idents: bool) => QCheck.Gen.t(string) =
  (~minimal_idents) =>
    // Currently there is an issue if the keyword is a prefix of another word.
    // `let ? = ina in ?`
    // Temporarily doing single char identifiers as a fix
    QCheck.Gen.(
      if (minimal_idents) {
        oneof([pure("x"), pure("y")]);
      } else {
        string_size(~gen=char_range('a', 'z'), int_range(1, 1));
      }
    );

/**
 * Generates an array of natural numbers of a given size.
 * Useful for generating recursive structures with arrays/lists.
 *
 * @param size - The size of the array, which also represents the number of elements in the array.
 * @return A QCheck generator that produces arrays of integers that have a size (sum of elements + num of elements) of n

 * This function is useful for size tracking purposes.
 */
let gen_sized_array = (n: int): QCheck.Gen.t(array(int)) =>
  QCheck.Gen.(
    let* list_size = n <= 1 ? pure(0) : int_range(2, n);
    switch (list_size) {
    | 0 => pure([||])
    | _ => nat_split(~size=list_size, n - list_size)
    }
  );

/**
 * Generates an array of natural numbers that is either empty or has a length of at least 2.
 * Useful for generating recursive structures with arrays/lists.
 *
 * @param n The size parameter used for generating the array.
 * @return A QCheck generator that produces arrays of integers that have a size (sum of elements + num of elements) of n
 *
 * This function is useful for size tracking purposes, similar to `gen_sized_array`.
 */
let gen_non_singleton_array = (n: int): QCheck.Gen.t(array(int)) =>
  QCheck.Gen.(
    let* list_size =
      frequency([(1, pure(0)), (n, n <= 1 ? pure(0) : int_range(2, n))]);

    switch (list_size) {
    | 0 => pure([||])
    | _ => nat_split(~size=list_size, n - list_size)
    }
  );

/**
 * Generates an array of natural numbers has a length of at least 1.
 * Useful for generating recursive structures with arrays/lists.
 *
 * @param n The size parameter used for generating the array.
 * @return A QCheck generator that produces arrays of integers that have a size (sum of elements + num of elements) of n
 *
 * This function is useful for size tracking purposes, similar to `gen_sized_array`.
 */
let gen_non_empty_array = (n: int): QCheck.Gen.t(array(int)) =>
  QCheck.Gen.(
    let* list_size = n <= 1 ? pure(0) : int_range(1, n);

    switch (list_size) {
    | 0 => pure([|0|]) // I'm a bit concerned about this not tracking size. But it seems to work in practice.
    | _ => nat_split(~size=list_size, n - list_size)
    }
  );

/**
 * Generates a random `tpat` value using QCheck.
 *
 * @return A generator for `tpat` values.
 */
let gen_tpat: (~minimal_idents: bool) => QCheck.Gen.t(tpat) =
  (~minimal_idents) =>
    QCheck.Gen.(
      let gen_ident = gen_ident(~minimal_idents);
      let gen_var = map(x => VarTPat(x), gen_ident);
      let gen_empty = pure(EmptyHoleTPat);
      // let gen_invalid = map(x => InvalidTPat(x), gen_ident); // Menhir parser doesn't actually support invalid tpat
      oneof([gen_var, gen_empty])
    );

/**
 * Generates a string literal for use in the program.
 * This generator produces strings that match the `string` pattern in the lexer.
 */
let gen_string_literal: QCheck.Gen.t(string) =
  // TODO This should be anything printable other than `"`
  QCheck.Gen.(string_small_of(char_range('a', 'z')));

let gen_label: QCheck.Gen.t(string) = gen_ident(~minimal_idents=false);

/**
 * Generates an expression of a given size.
 *
 * @param n The size of the expression to generate.
 * @return A generator for expressions of the specified size.
 *
 * This function is currently used for property tests between MakeTerm and the Menhir parser,
 * so it's not currently set up to generate every possible expression.
 */
let rec gen_exp_sized = (~minimal_idents: bool, n: int): QCheck.Gen.t(exp) => {
  open QCheck.Gen;
  let gen_constructor_ident = gen_constructor_ident(~minimal_idents);
  let gen_ident = gen_ident(~minimal_idents);

  let gen_pat_sized = n => gen_pat_sized(~minimal_idents, n);
  let gen_typ_sized = n => gen_typ_sized(~minimal_idents, n);
  let gen_tpat = gen_tpat(~minimal_idents);
  let leaf =
    oneof([
      map(x => Atom(Int(x |> Bigint.of_int)), small_int),
      map(x => Atom(String(x)), gen_string_literal),
      map(x => Atom(Float(x)), QCheck.pos_float.gen), // Floats are positive because we use UnOp minus
      map(x => Var(x), gen_ident),
      map(x => Atom(Bool(x)), bool),
      pure(EmptyHole),
      pure(TupleExp([])),
      pure(ListExp([])),
      map(x => Constructor(x, None), gen_constructor_ident),
    ]);
  fix(
    (self: int => t(exp), n) => {
      switch (n) {
      | n when n <= 1 => leaf
      | _ =>
        oneof([
          leaf,
          {
            let* sizes = gen_sized_array(n);
            let+ exps = flatten_a(Array.map((n: int) => self(n), sizes));
            ListExp(Array.to_list(exps));
          },
          {
            let* sizes = gen_non_singleton_array(n);
            let+ exps =
              flatten_a(
                Array.map(
                  (n: int) =>
                    oneof([
                      {
                        let* l = gen_label;
                        let+ e = self(n - 1);
                        TupLabel(Label(l), e);
                      },
                      self(n),
                    ]),
                  sizes,
                ),
              );
            TupleExp(Array.to_list(exps));
          },
          {
            let+ inner = self(n - 1);
            Test(inner);
          },
          {
            let* op = gen_bin_op;
            let* e1 = self((n - 1) / 2);
            let+ e2 = self((n - 1) / 2);
            BinExp(e1, op, e2);
          },
          {
            let* op = gen_op_un;
            let+ e = self(n - 1);
            UnOp(op, e);
          },
          {
            let* e1 = self((n - 1) / 3);
            let* e2 = self((n - 1) / 3);
            let+ e3 = self((n - 1) / 3);
            If(e1, e2, e3);
          },
          {
            let* p = gen_pat_sized((n - 1) / 3);
            let* e1 = self((n - 1) / 3);
            let+ e2 = self((n - 1) / 3);
            Let(p, e1, e2);
          },
          {
            let* p = gen_pat_sized((n - 1) / 2);
            let+ e = self((n - 1) / 2);
            Fun(p, e, None);
          },
          {
            let case = n => {
              let p = gen_pat_sized((n - 1) / 2);
              let e = self((n - 1) / 2);
              tup2(p, e);
            };
            let* e = self((n - 1) / 2);
            let* sizes = gen_sized_array((n - 1) / 2);
            let+ cases = flatten_a(Array.map(case, sizes));
            CaseExp(e, Array.to_list(cases));
          },
          {
            let* e1 = self((n - 1) / 2);
            let+ e2 =
              frequency([(5, self((n - 1) / 2)), (1, return(Deferral))]);
            ApExp(e1, e2);
          },
          {
            let* p = gen_pat_sized((n - 1) / 2);
            let+ e = self((n - 1) / 2);
            FixF(p, e);
          },
          {
            let* fa = gen_filter_action;
            let* e1 = self((n - 1) / 2);
            let+ e2 = self((n - 1) / 2);
            Filter(fa, e1, e2);
          },
          {
            let* e1 = self((n - 1) / 2);
            let+ e2 = self((n - 1) / 2);
            Seq(e1, e2);
          },
          {
            let* e1 = self((n - 1) / 2);
            let+ e2 = self((n - 1) / 2);
            Cons(e1, e2);
          },
          {
            let* e1 = self((n - 1) / 2);
            let+ e2 = self((n - 1) / 2);
            ListConcat(e1, e2);
          },
          {
            let* tp = gen_tpat;
            let+ e = self(n - 1);
            TypFun(tp, e);
          },
          {
            let* t = gen_typ_sized((n - 1) / 2);
            let+ e = self((n - 1) / 2);
            TypAp(e, t);
          },
          {
            let* tp = gen_tpat;
            let* t = gen_typ_sized((n - 1) / 2);
            let+ e = self((n - 1) / 2);
            TyAlias(tp, t, e);
          },
        ])
      }
    },
    n,
  );
}
/**
 * Generates a type of a given size.
 *
 * @param n The size of the type to generate.
 * @return A generator for types of the specified size.
 *
 * This function is currently used for property tests between MakeTerm and the Menhir parser,
 * so it's not currently set up to generate every possible type.
 */
and gen_typ_sized: (~minimal_idents: bool, int) => QCheck.Gen.t(typ) =
  (~minimal_idents, n) =>
    QCheck.Gen.(
      let gen_ident = gen_ident(~minimal_idents);
      let gen_constructor_ident = gen_constructor_ident(~minimal_idents);
      let gen_tpat = gen_tpat(~minimal_idents);
      let leaf_nodes =
        oneof([
          return(StringType),
          return(FloatType),
          return(BoolType),
          return(TupleType([])),
          return(UnknownType(EmptyHole)), // Only doing emptyhole because internal doesn't have a distinct representation in ExpToSegment
          map(x => SumTyp([Variant(x, None)]), gen_constructor_ident),
        ]);
      fix(
        (self, n) =>
          switch (n) {
          | n when n <= 1 => leaf_nodes
          | _ =>
            oneof([
              leaf_nodes,
              {
                let* sizes = gen_non_singleton_array(n - 1);
                let+ typs =
                  flatten_a(
                    Array.map(
                      (size: int) =>
                        oneof([
                          self(size),
                          {
                            let* l = gen_label;
                            let+ t = self(size);
                            TupLabelType(LabelType(l), t);
                          },
                        ]),
                      sizes,
                    ),
                  );
                TupleType(Array.to_list(typs));
              },
              {
                let+ t = self(n - 1);
                ArrayType(t);
              },
              {
                let* t1 = self((n - 1) / 2);
                let+ t2 = self((n - 1) / 2);
                ArrowType(t1, t2);
              },
              {
                let+ ident = gen_ident;
                TypVar(ident);
              },
              {
                let* gen_tpat = gen_tpat;
                let+ t = self(n - 1);
                PolyType(gen_tpat, t);
              },
              {
                let* gen_tpat = gen_tpat;
                let+ t = self(n - 1);
                RecType(gen_tpat, t);
              },
              {
                let* t1 = self((n - 1) / 2);
                let+ t2 = self((n - 1) / 2);
                ApTyp(t1, t2);
              },
              {
                let* sizes = gen_non_empty_array(n - 1);
                let+ sumterms =
                  flatten_a(
                    Array.map(
                      (n: int) => {
                        frequency([
                          (1, return(BadEntry(UnknownType(EmptyHole)))),
                          (
                            5,
                            {
                              let* optional_typ = option(self(n - 1));
                              let+ constructor = gen_constructor_ident;
                              Variant(constructor, optional_typ);
                            },
                          ),
                        ])
                      },
                      sizes,
                    ),
                  );

                SumTyp(Array.to_list(sumterms));
              },
            ])
          },
        n,
      )
    )

/**
 * Generates an pattern of a given size.
 *
 * @param n The size of the pattern to generate.
 * @return A generator for expressions of the specified size.
 *
 * This function is currently used for property tests between MakeTerm and the Menhir parser,
 * so it's not currently set up to generate every possible pattern.
 */
and gen_pat_sized: (~minimal_idents: bool, int) => QCheck.Gen.t(pat) =
  (~minimal_idents, n) =>
    QCheck.Gen.(
      let gen_ident = gen_ident(~minimal_idents);
      let gen_constructor_ident = gen_constructor_ident(~minimal_idents);
      let gen_typ_sized = n => gen_typ_sized(~minimal_idents, n);
      fix(
        (self, n) => {
          let leaf_nodes =
            oneof([
              return(WildPat),
              return(EmptyHolePat),
              map(x => AtomPat(Int(x |> Bigint.of_int)), small_int),
              map(x => AtomPat(Float(x)), QCheck.pos_float.gen),
              map(x => VarPat(x), gen_ident),
              map(x => AtomPat(String(x)), gen_string_literal),
              map(x => AtomPat(Bool(x)), bool),
              map(x => ConstructorPat(x, None), gen_constructor_ident),
              return(TuplePat([])),
              return(ListPat([])),
            ]);

          switch (n) {
          | n when n <= 1 => leaf_nodes
          | _ =>
            oneof([
              leaf_nodes,
              {
                let* p1 = self((n - 1) / 2);
                let+ p2 = self((n - 1) / 2);
                ConsPat(p1, p2);
              },
              {
                let* sizes = gen_non_singleton_array(n - 1);
                let+ pats =
                  flatten_a(
                    Array.map(
                      (n: int) =>
                        oneof([
                          self(n),
                          {
                            let* l = gen_label;
                            let+ p = self(n - 1);
                            TupLabelPat(LabelPat(l), p);
                          },
                        ]),
                      sizes,
                    ),
                  );
                TuplePat(Array.to_list(pats));
              },
              {
                let* sizes = gen_sized_array(n - 1);
                let+ pats =
                  flatten_a(Array.map((size: int) => self(size), sizes));
                ListPat(Array.to_list(pats));
              },
              {
                let* constructor = gen_constructor_ident;
                let+ p = self(n - 1);
                ApPat(ConstructorPat(constructor, None), p);
              }, // The parser only handles ApPat with a constructor
              {
                let* p = self((n - 1) / 2);
                let+ t1 = gen_typ_sized((n - 1) / 2);
                AscPat(p, t1);
              },
            ])
          };
        },
        n,
      )
    );

let shrink_non_empty_string: QCheck.Shrink.t(string) =
  x => QCheck.Shrink.(filter(x => String.length(x) != 0, string, x));

let rec shrink_exp: QCheck.Shrink.t(exp) =
  QCheck.(
    (exp: exp) =>
      Iter.(
        switch (exp) {
        | Atom(a) =>
          switch (a) {
          | Int(i) =>
            switch (Bigint.to_int(i)) {
            | Some(i) =>
              Shrink.int(i) >|= ((i: int) => Atom(Int(Bigint.of_int(i))))
            | None => Iter.empty
            }
          | String(s) =>
            Shrink.string(s) >|= ((s: string) => Atom(String(s)))
          | Bool(b) => Shrink.bool(b) >|= ((b: bool) => Atom(Bool(b)))
          | Nat(n) =>
            if (Bigint.(<)(n, Bigint.of_int(2))) {
              Iter.empty;
            } else {
              return(Atom(Nat(Bigint.(/)(n, Bigint.of_int(2)))));
            }
          | SInt(i) => Shrink.int(i) >|= ((i: int) => Atom(SInt(i)))
          | _ => Iter.empty
          }
        | Var(x) => shrink_non_empty_string(x) >|= ((x: string) => Var(x)) // TODO This isn't great for vars
        | Constructor(_, _) => Iter.empty // TODO Constructors. Shrinking needs to preserve constructor ident format
        | ListExp(l) =>
          let* shrunk = Shrink.list(l, ~shrink=shrink_exp);
          switch (shrunk) {
          | [x] => return(ListExp(shrunk)) <+> Iter.return(x)
          | _ => return(ListExp(shrunk))
          };
        | TupleExp(l) =>
          if (List.length(l) <= 1) {
            Iter.empty;
          } else {
            let* shrunk = Shrink.list(l, ~shrink=shrink_exp);
            switch (shrunk) {
            | [] => Iter.return(TupleExp([]))
            | [x] => Iter.return(x)
            | _ => return(TupleExp(shrunk))
            };
          }
        | BinExp(e1, op, e2) =>
          {
            of_list([e1, e2]);
          }
          <+> {
            shrink_exp(e1) >|= (e1 => BinExp(e1, op, e2));
          }
          <+> {
            shrink_exp(e2) >|= (e2 => BinExp(e1, op, e2));
          }
        | UnOp(op, e) =>
          return(e)
          <+> {
            let* shrunk = shrink_exp(e);
            return(UnOp(op, shrunk));
          }
        | Let(p, e1, e2) =>
          of_list([e1, e2])
          <+> {
            let* shrunk = shrink_exp(e1);
            return(Let(p, shrunk, e2));
          }
          <+> {
            let* shrunk = shrink_exp(e2);
            return(Let(p, e1, shrunk));
          }
          <+> {
            let* shrunk = shrink_pat(p);
            return(Let(shrunk, e1, e2));
          }
        | Fun(p, e, name) =>
          return(e)
          <+> {
            let* shrunk = shrink_exp(e);
            return(Fun(p, shrunk, name): exp);
          }
          <+> {
            let* shrunk = shrink_pat(p);
            return(Fun(shrunk, e, name): exp);
          }
        | CaseExp(e, cases) =>
          {
            return(e);
          }
          <+> {
            let* shrunk = shrink_exp(e);
            return(CaseExp(shrunk, cases));
          }
          <+> {
            let shrink_case: QCheck.Shrink.t((pat, exp)) =
              QCheck.(
                (
                  ((pat, exp)) =>
                    Iter.(
                      {
                        let* pat = shrink_pat(pat);
                        return((pat, exp));
                      }
                      <+> {
                        let* exp = shrink_exp(exp);
                        return((pat, exp));
                      }
                    )
                )
              );
            let* shrunk = Shrink.list(cases, ~shrink=shrink_case);
            return(CaseExp(e, shrunk));
          }
        | Label(l) =>
          shrink_non_empty_string(l) >|= ((l: string) => Label(l))
        | TupLabel(e1, e2) =>
          {
            return(
              e2 // e1 is a label
            );
          }
          <+> {
            let* shrunk = shrink_exp(e1);
            return(TupLabel(shrunk, e2));
          }
          <+> {
            let* shrunk = shrink_exp(e2);
            return(TupLabel(e1, shrunk));
          }
        | Dot(e1, e2) =>
          {
            return(
              e1 // e2 is a label
            );
          }
          <+> {
            let* shrunk = shrink_exp(e1);
            return(Dot(shrunk, e2));
          }
          <+> {
            let* shrunk = shrink_exp(e2);
            return(Dot(e1, shrunk));
          }
        | ApExp(e1, e2) =>
          {
            of_list([e1, e2]);
          }
          <+> {
            let* shrunk = shrink_exp(e1);
            return(ApExp(shrunk, e2));
          }
          <+> {
            let* shrunk = shrink_exp(e2);
            return(ApExp(e1, shrunk));
          }
        | TypAp(e, t) =>
          {
            return(e);
          }
          <+> {
            let* shrunk = shrink_exp(e);
            return(TypAp(shrunk, t));
          }
          <+> {
            let* shrunk = shrink_typ(t);
            return(TypAp(e, shrunk));
          }
        | FixF(p, e) =>
          return(e)
          <+> {
            let* shrunk = shrink_exp(e);
            return(FixF(p, shrunk));
          }
          <+> {
            let* shrunk = shrink_pat(p);
            return(FixF(shrunk, e));
          }
        | Asc(e, t) =>
          return(e)
          <+> {
            let* shrunk = shrink_exp(e);
            return(Asc(shrunk, t));
          }
          <+> {
            let* shrunk = shrink_typ(t);
            return(Asc(e, shrunk));
          }
        | Filter(fa, e1, e2) =>
          {
            of_list([e1, e2]);
          }
          <+> {
            let* shrunk = shrink_exp(e1);
            return(Filter(fa, shrunk, e2));
          }
          <+> {
            let* shrunk = shrink_exp(e2);
            return(Filter(fa, e1, shrunk));
          }
        | Seq(e1, e2) =>
          {
            of_list([e1, e2]);
          }
          <+> {
            let* shrunk = shrink_exp(e1);
            return(Seq(shrunk, e2));
          }
          <+> {
            let* shrunk = shrink_exp(e2);
            return(Seq(e1, shrunk));
          }
        | Test(e) =>
          return(e)
          <+> {
            let* shrunk = shrink_exp(e);
            return(Test(shrunk));
          }
        | HintedTest(e1, e2) =>
          {
            of_list([e1, e2]);
          }
          <+> {
            let* shrunk = shrink_exp(e1);
            return(HintedTest(shrunk, e2));
          }
          <+> {
            let* shrunk = shrink_exp(e2);
            return(HintedTest(e1, shrunk));
          }
        | Deferral => Iter.empty
        | TypFun(tpat, e) =>
          return(e)
          <+> {
            let* shrunk = shrink_exp(e);
            return(TypFun(tpat, shrunk));
          } // Not worth shrinking tpat
        | Cons(e1, e2) =>
          {
            of_list([e1, e2]);
          }
          <+> {
            let* shrunk = shrink_exp(e1);
            return(Cons(shrunk, e2));
          }
          <+> {
            let* shrunk = shrink_exp(e2);
            return(Cons(e1, shrunk));
          }
        | ListConcat(e1, e2) =>
          {
            of_list([e1, e2]);
          }
          <+> {
            let* shrunk = shrink_exp(e1);
            return(ListConcat(shrunk, e2));
          }
          <+> {
            let* shrunk = shrink_exp(e2);
            return(ListConcat(e1, shrunk));
          }
        | DynamicErrorHole(e, s) =>
          return(e)
          <+> {
            let* shrunk = shrink_exp(e);
            return(DynamicErrorHole(shrunk, s));
          }
        | TyAlias(tpat, t, e) =>
          return(e)
          <+> {
            let* shrunk = shrink_exp(e);
            return(TyAlias(tpat, t, shrunk));
          }
          <+> {
            let* shrunk = shrink_typ(t);
            return(TyAlias(tpat, shrunk, e));
          }
        | Use(t, e) =>
          return(e)
          <+> {
            let* shrunk = shrink_exp(e);
            return(Use(t, shrunk));
          }
          <+> {
            let* shrunk = shrink_typ(t);
            return(Use(shrunk, e));
          }
        | If(e1, e2, e3) =>
          {
            of_list([e1, e2, e3]);
          }
          <+> {
            let* shrunk = shrink_exp(e1);
            return(If(shrunk, e2, e3));
          }
          <+> {
            let* shrunk = shrink_exp(e2);
            return(If(e1, shrunk, e3));
          }
          <+> {
            let* shrunk = shrink_exp(e3);
            return(If(e1, e2, shrunk));
          }
        | IndicationExp(_)
        | EmptyHole
        | BuiltinFun(_)
        | Undefined
        | InvalidExp(_) => Iter.empty
        }
      )
  )
and shrink_pat: QCheck.Shrink.t(pat) =
  QCheck.(
    (pat: pat) =>
      Iter.(
        switch (pat) {
        | AtomPat(a) =>
          switch (a) {
          | Int(i) =>
            switch (Bigint.to_int(i)) {
            | Some(i) =>
              Shrink.int(i)
              >|= ((i: int) => AtomPat(Int(Bigint.of_int(i))))
            | None => Iter.empty
            }
          | String(s) =>
            Shrink.string(s) >|= ((s: string) => AtomPat(String(s)))
          | Bool(b) => Shrink.bool(b) >|= ((b: bool) => AtomPat(Bool(b)))
          | Nat(n) =>
            if (Bigint.(<)(n, Bigint.of_int(2))) {
              Iter.empty;
            } else {
              return(AtomPat(Nat(Bigint.(/)(n, Bigint.of_int(2)))));
            }
          | SInt(i) => Shrink.int(i) >|= ((i: int) => AtomPat(SInt(i)))
          | _ => Iter.empty
          }
        | VarPat(x) =>
          shrink_non_empty_string(x) >|= ((x: string) => VarPat(x))
        | ConstructorPat(_) => Iter.empty // Needs to preserve constructor ident
        | ListPat(l) =>
          let* shrunk = Shrink.list(l, ~shrink=shrink_pat);
          switch (shrunk) {
          | [x] => return(ListPat(shrunk)) <+> Iter.return(x)
          | _ => return(ListPat(shrunk))
          };
        | AscPat(p, t) =>
          return(p)
          <+> {
            let* shrunk = shrink_pat(p);
            return(AscPat(shrunk, t));
          }
          <+> {
            let* shrunk = shrink_typ(t);
            return(AscPat(p, shrunk));
          }
        | ApPat(p1, p2) =>
          {
            of_list([p1, p2]);
          }
          <+> {
            let* shrunk = shrink_pat(p1);
            return(ApPat(shrunk, p2));
          }
          <+> {
            let* shrunk = shrink_pat(p2);
            return(ApPat(p1, shrunk));
          }
        | TuplePat(l) =>
          let* shrunk = Shrink.list(l, ~shrink=shrink_pat);
          switch (shrunk) {
          | [] => Iter.return(TuplePat([]))
          | [x] => Iter.return(x)
          | _ => return(TuplePat(shrunk))
          };
        | ConsPat(p1, p2) =>
          {
            of_list([p1, p2]);
          }
          <+> {
            let* shrunk = shrink_pat(p1);
            return(ConsPat(shrunk, p2));
          }
          <+> {
            let* shrunk = shrink_pat(p2);
            return(ConsPat(p1, shrunk));
          }
        | TupLabelPat(p1, p2) =>
          {
            return(
              p2 // p1 is a label
            );
          }
          <+> {
            let* shrunk = shrink_pat(p1);
            return(TupLabelPat(shrunk, p2));
          }
          <+> {
            let* shrunk = shrink_pat(p2);
            return(TupLabelPat(p1, shrunk));
          }
        | LabelPat(l) =>
          shrink_non_empty_string(l) >|= ((l: string) => LabelPat(l))
        | InvalidPat(_)
        | IndicationPat(_)
        | WildPat
        | EmptyHolePat => Iter.empty
        }
      )
  )
and shrink_typ: QCheck.Shrink.t(typ) =
  QCheck.(
    (typ: typ) =>
      Iter.(
        switch (typ) {
        | SumTyp(l) =>
          let shrink_sumterm: QCheck.Shrink.t(sumterm) =
            QCheck.(
              (
                (sumterm: sumterm) =>
                  Iter.(
                    switch (sumterm) {
                    | Variant(_) => Iter.empty
                    | BadEntry(t) =>
                      let* shrunk = shrink_typ(t);
                      return(BadEntry(shrunk));
                    }
                  )
              )
            );
          let* shrunk = Shrink.list(l, ~shrink=shrink_sumterm);
          switch (shrunk) {
          | [] => Iter.empty
          | _ => return(SumTyp(shrunk))
          };
        | TupleType(l) =>
          let* shrunk = Shrink.list(l, ~shrink=shrink_typ);
          switch (shrunk) {
          | [x] => Iter.return(x)
          | _ => return(TupleType(shrunk))
          };
        | ArrayType(t) =>
          return(t)
          <+> {
            let* shrunk = shrink_typ(t);
            return(ArrayType(shrunk));
          }
        | ArrowType(t1, t2) =>
          of_list([t1, t2])
          <+> {
            let* shrunk1 = shrink_typ(t1);
            return(ArrowType(shrunk1, t2));
          }
          <+> {
            let* shrunk2 = shrink_typ(t2);
            return(ArrowType(t1, shrunk2));
          }
        | TypVar(x) => Shrink.string(x) >|= ((x: string) => TypVar(x))
        | PolyType(tpat, t) =>
          let* shrunk = shrink_typ(t);
          return(PolyType(tpat, shrunk));
        | RecType(tpat, t) =>
          let* shrunk = shrink_typ(t);
          return(RecType(tpat, shrunk));
        | LabelType(x) =>
          shrink_non_empty_string(x) >|= ((x: string) => LabelType(x))
        | TupLabelType(t1, t2) =>
          return(t2)
          <+> {
            let* shrunk1 = shrink_typ(t1);
            return(TupLabelType(shrunk1, t2));
          }
          <+> {
            let* shrunk2 = shrink_typ(t2);
            return(TupLabelType(t1, shrunk2));
          }
        | ApTyp(t1, t2) =>
          of_list([t1, t2])
          <+> {
            let* shrunk1 = shrink_typ(t1);
            return(ApTyp(shrunk1, t2));
          }
          <+> {
            let* shrunk2 = shrink_typ(t2);
            return(ApTyp(t1, shrunk2));
          }
        | IndicationTyp(_)
        | IntType
        | SIntType
        | StringType
        | FloatType
        | BoolType
        | NatType
        | UnknownType(_)
        | InvalidTyp(_) => Iter.empty
        }
      )
  );
let arb_typ = (~minimal_idents=false, size) =>
  QCheck.make(
    ~print=show_typ,
    ~shrink=shrink_typ,
    gen_typ_sized(~minimal_idents, size),
  );
let arb_exp = (~minimal_idents=false, size) =>
  QCheck.make(
    ~print=show_exp,
    ~shrink=shrink_exp,
    gen_exp_sized(~minimal_idents, size),
  );
