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
  | GreaterThanOrEqual
  | Equals
  | NotEquals;

[@deriving (show({with_path: false}), sexp, qcheck, eq)]
type op_bin_string =
  | Concat
  | Equals;

[@deriving (show({with_path: false}), sexp, qcheck, eq)]
type bin_op =
  | IntOp(op_bin_int)
  | FloatOp(op_bin_float)
  | StringOp(op_bin_string)
  | BoolOp(op_bin_bool);

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
  | StringType
  | FloatType
  | BoolType
  | UnitType
  | SumTyp(sumtype)
  | UnknownType(typ_provenance)
  | TupleType(list(typ))
  | ArrayType(typ)
  | ArrowType(typ, typ)
  | TypVar(string)
  | InvalidTyp(string)
  | ForallType(tpat, typ)
  | RecType(tpat, typ)
and sumterm =
  | Variant(string, option(typ))
  | BadEntry(typ)
and sumtype = list(sumterm);

[@deriving (show({with_path: false}), sexp, eq)]
type pat =
  | CastPat(pat, typ, typ)
  | EmptyHolePat
  | WildPat
  | IntPat(int)
  | FloatPat(float)
  | VarPat(string)
  | ConstructorPat(string, typ)
  | StringPat(string)
  | TuplePat(list(pat))
  | BoolPat(bool)
  | ConsPat(pat, pat)
  | ListPat(list(pat))
  | ApPat(pat, pat)
  | InvalidPat(string); // Menhir parser doesn't actually support invalid pats

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
  | Int(int)
  | Float
      // This equality condition is used to say that two floats are equal if they are equal in the ExpToSegment serialization
      (
        [@equal
          (a, b) => Printf.(sprintf("%.12f", a) == sprintf("%.12f", b))
        ] float,
      )
  | Var(string)
  | Constructor(string, typ)
  | String(string)
  | ListExp(list(exp))
  | TupleExp(list(exp))
  | BinExp(exp, bin_op, exp)
  | UnOp(op_un, exp)
  | Let(pat, exp, exp)
  | Fun(pat, exp, option(string))
  | CaseExp(exp, list((pat, exp)))
  | ApExp(exp, exp)
  | FixF(pat, exp)
  | Bool(bool)
  | Cast(exp, typ, typ)
  | FailedCast(exp, typ, typ)
  | EmptyHole
  | Filter(filter_action, exp, exp)
  | BuiltinFun(string)
  | Undefined
  | Seq(exp, exp)
  | Test(exp)
  | Deferral
  | TypFun(tpat, exp)
  | Cons(exp, exp)
  | ListConcat(exp, exp)
  | If(exp, exp, exp)
  | InvalidExp(string)
  | TypAp(exp, typ)
  | DynamicErrorHole(exp, string)
  | TyAlias(tpat, typ, exp);

let arb_constructor_ident =
  QCheck.
    // TODO handle full constructor ident including nums
    (
      let leading = Gen.char_range('A', 'Z');
      let tail =
        string_gen_of_size(
          Gen.int_range(1, 4),
          QCheck.Gen.char_range('a', 'z'),
        );
      QCheck.map(
        ident =>
          // if ident is a keyword add a suffix
          switch (ident) {
          | "Int"
          | "Float"
          | "String"
          | "Unknown"
          | "Internal"
          | "Bool" => "keyword"
          | _ => ident
          },
        make(
          ~print=t => t,
          Gen.map2((l, t) => String.make(1, l) ++ t, leading, tail.gen),
        ),
      )
    );

// ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
// Currently an issue if the keyword is a prefix of another word. `let ? = ina in ?`
// Temporarily doing single char identifiers as a fix
let arb_ident =
  QCheck.(
    // TODO make this support full indent instead of just lower alpha
    QCheck.map(
      ident =>
        // if ident is a keyword add a suffix
        switch (ident) {
        | "let"
        | "if"
        | "pause"
        | "debug"
        | "hide"
        | "eval"
        | "rec"
        | "in" => "keyword"
        | _ => ident
        },
      string_gen_of_size(
        Gen.int_range(1, 1),
        QCheck.Gen.char_range('a', 'z'),
      ),
    )
  );

let sized_arr = (n: int): QCheck.Gen.t(array(int)) =>
  QCheck.Gen.(
    let* list_size = n <= 1 ? pure(0) : int_range(2, n);
    switch (list_size) {
    | 0 => pure([||])
    | _ => nat_split(~size=list_size, n - list_size)
    }
  );

let non_single_element_arr = (n: int): QCheck.Gen.t(array(int)) =>
  QCheck.Gen.(
    let* list_size =
      frequency([(1, pure(0)), (n, n <= 1 ? pure(0) : int_range(2, n))]);

    switch (list_size) {
    | 0 => pure([||])
    | _ => nat_split(~size=list_size, n - list_size)
    }
  );

let non_empty_arr = (n: int): QCheck.Gen.t(array(int)) =>
  QCheck.Gen.(
    let* list_size = n <= 1 ? pure(0) : int_range(1, n);

    switch (list_size) {
    | 0 => pure([|0|]) // I'm a bit concerned about this not tracking size. But it seems to work in practice.
    | _ => nat_split(~size=list_size, n - list_size)
    }
  );

let gen_tpat: QCheck.Gen.t(tpat) =
  QCheck.Gen.(
    let gen_var = map(x => VarTPat(x), arb_ident.gen);
    let gen_empty = pure(EmptyHoleTPat);
    // let gen_invalid = map(x => InvalidTPat(x), arb_ident.gen);
    oneof([gen_var, gen_empty])
  );

// TODO This should be anything printable other than `"`
let gen_literal_string: QCheck.Gen.t(string) = QCheck.Gen.(string_small_of(char_range('a', 'z')));

let rec gen_exp_sized = (n: int): QCheck.Gen.t(exp) =>
  QCheck.Gen.(
    let leaf =
      oneof([
        map(x => Int(x), small_int),
        map(x => String(x), gen_literal_string),
        map(x => Float(x), QCheck.pos_float.gen), // Floats are positive because we use UnOp minus
        map(x => Var(x), arb_ident.gen),
        map(x => Bool(x), bool),
        pure(EmptyHole),
        pure(TupleExp([])),
        pure(ListExp([])),
      ]);
    fix(
      (self: int => t(exp), n) => {
        switch (n) {
        | 0
        | 1 => leaf
        | _ =>
          oneof([
            leaf,
            {
              let* sizes = sized_arr(n);
              let+ exps =
                flatten_a(Array.map((size: int) => self(size), sizes));
              ListExp(Array.to_list(exps));
            },
            {
              let* sizes = non_single_element_arr(n);
              let+ exps =
                flatten_a(Array.map((size: int) => self(size), sizes));
              TupleExp(Array.to_list(exps));
            },
            {
              let+ inner = self(n - 1);
              Test(inner);
            },
            {
              let+ name = arb_constructor_ident.gen;
              Constructor(name, UnknownType(Internal));
            },
            {
              let* op = gen_bin_op;
              let* e1 = self((n - 1) / 2);
              let+ e2 = self((n - 1) / 2);
              BinExp(e1, op, e2);
            },
            {
              // TODO ExpToSegment broken for UnOp
              // {
              //   let* op = gen_op_un;
              //   let+ e = self((n - 1) / 2);
              //   UnOp(op, e);
              // },

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
                let p = gen_pat_sized(n / 2);
                let e = self(n / 2);
                tup2(p, e);
              };
              let* e = self(n - 1 / 2);
              let* sizes = sized_arr(n - 1 / 2);
              let+ cases = flatten_a(Array.map(case, sizes));
              CaseExp(e, Array.to_list(cases));
            },
            {
              let* e1 = self((n - 1) / 2);
              let+ e2 =
                frequency([
                  (5, self((n - 1) / 2)),
                  (1, return(Deferral)),
                ]);
              ApExp(e1, e2);
            },
            {
              let* p = gen_pat_sized((n - 1) / 2);
              let+ e = self((n - 1) / 2);
              FixF(p, e);
            },
            {
              let* fa = gen_filter_action;
              let* e1 = self(n - 1);
              let+ e2 = self(n - 1);
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
    )
  )
and gen_typ_sized: int => QCheck.Gen.t(typ) =
  n =>
    QCheck.Gen.(
      let leaf_nodes =
        oneof([
          return(StringType),
          return(FloatType),
          return(BoolType),
          return(UnitType),
          map(x => UnknownType(x), arb_typ_provenance.gen),
          map(x => SumTyp([Variant(x, None)]), arb_constructor_ident.gen),
        ]);
      fix(
        (self, n) =>
          switch (n) {
          | 0 => leaf_nodes
          | _ =>
            oneof([
              leaf_nodes,
              {
                let* sizes = non_single_element_arr(n);
                let+ typs =
                  flatten_a(Array.map((size: int) => self(size), sizes));
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
                let+ ident = arb_ident.gen;
                TypVar(ident);
              },
              {
                let* gen_tpat = gen_tpat;
                let+ t = self(n - 1);
                ForallType(gen_tpat, t);
              },
              {
                let* gen_tpat = gen_tpat;
                let+ t = self(n - 1);
                RecType(gen_tpat, t);
              },
              {
                let* sizes = non_empty_arr(n - 1);
                let+ sumterms =
                  flatten_a(
                    Array.map(
                      (size: int) => {
                        frequency([
                          (1, return(BadEntry(UnknownType(EmptyHole)))),
                          (
                            5,
                            {
                              let* optional_typ = option(self(size));
                              let+ constructor = arb_constructor_ident.gen;
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
and gen_pat_sized: int => QCheck.Gen.t(pat) =
  n =>
    QCheck.Gen.(
      fix(
        (self, n) => {
          let leaf_nodes =
            oneof([
              return(WildPat),
              return(EmptyHolePat),
              map(x => IntPat(x), small_int),
              map(x => FloatPat(x), QCheck.pos_float.gen),
              map(x => VarPat(x), arb_ident.gen),
              map(x => StringPat(x), gen_literal_string),
              map(x => BoolPat(x), bool),
              map(
                x => ConstructorPat(x, UnknownType(Internal)),
                arb_constructor_ident.gen,
              ),
              return(TuplePat([])),
              return(ListPat([])),
            ]);

          switch (n) {
          | 0 => leaf_nodes
          | _ =>
            oneof([
              leaf_nodes,
              {
                let* p1 = self((n - 1) / 2);
                let+ p2 = self((n - 1) / 2);
                ConsPat(p1, p2);
              },
              {
                let* sizes = non_single_element_arr(n - 1);
                let+ pats =
                  flatten_a(Array.map((size: int) => self(size), sizes));
                TuplePat(Array.to_list(pats));
              },
              {
                let* sizes = sized_arr(n - 1);
                let+ pats =
                  flatten_a(Array.map((size: int) => self(size), sizes));
                ListPat(Array.to_list(pats));
              },
              {
                let* constructor = arb_constructor_ident.gen;
                let+ p = self(n - 1);
                ApPat(
                  ConstructorPat(constructor, UnknownType(Internal)),
                  p,
                );
              }, // The parser only handles ApPat with a constructor
              {
                let* p = self((n - 1) / 3);
                let* t1 = gen_typ_sized((n - 1) / 3);
                let+ t2 = gen_typ_sized((n - 1) / 3);
                CastPat(p, t1, t2);
              },
            ])
          };
        },
        n,
      )
    );
// TODO Printers, shrinkers stuff
