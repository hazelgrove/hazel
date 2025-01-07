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
  | FloatPat(
      [@equal (a, b) => Printf.(sprintf("%f", a) == sprintf("%f", b))] float,
    )
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
        [@equal (a, b) => Printf.(sprintf("%f", a) == sprintf("%f", b))] float,
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

/**
 * Generates a random CONSTRUCTOR_IDENT string. Used for CONSTRUCTOR_IDENT in the lexer.
 *
 * @return A QCheck generator for Constructor Identifier.
 *
 * ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
 */
let gen_constructor_ident: QCheck.Gen.t(string) =
  // TODO handle full constructor ident including nums
  QCheck.Gen.(
    let* leading = char_range('A', 'Z');
    let+ tail = string_size(~gen=char_range('a', 'z'), int_range(1, 4));
    let ident = String.make(1, leading) ++ tail;
    if (List.exists(a => a == ident, Haz3lcore.Form.base_typs)) {
      "Keyword";
    } else {
      ident;
    }
  );

/**
 * Generates a random IDENT string. Used for IDENT in the lexer.
 *
 * @return A QCheck generator for Identifier.
 *
 * ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
 */
let gen_ident: QCheck.Gen.t(string) =
  // Currently there is an issue if the keyword is a prefix of another word.
  // `let ? = ina in ?`
  // Temporarily doing single char identifiers as a fix
  QCheck.Gen.(string_size(~gen=char_range('a', 'z'), int_range(1, 1)));

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
let gen_tpat: QCheck.Gen.t(tpat) =
  QCheck.Gen.(
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

/**
 * Generates an expression of a given size.
 *
 * @param n The size of the expression to generate.
 * @return A generator for expressions of the specified size.
 *
 * This function is currently used for property tests between MakeTerm and the Menhir parser,
 * so it's not currently set up to generate every possible expression.
 */
let rec gen_exp_sized = (n: int): QCheck.Gen.t(exp) =>
  QCheck.Gen.(
    let leaf =
      oneof([
        map(x => Int(x), small_int),
        map(x => String(x), gen_string_literal),
        map(x => Float(x), QCheck.pos_float.gen), // Floats are positive because we use UnOp minus
        map(x => Var(x), gen_ident),
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
              let* sizes = gen_sized_array(n);
              let+ exps =
                flatten_a(Array.map((size: int) => self(size), sizes));
              ListExp(Array.to_list(exps));
            },
            {
              let* sizes = gen_non_singleton_array(n);
              let+ exps =
                flatten_a(Array.map((size: int) => self(size), sizes));
              TupleExp(Array.to_list(exps));
            },
            {
              let+ inner = self(n - 1);
              Test(inner);
            },
            {
              let+ name = gen_constructor_ident;
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
              let* sizes = gen_sized_array(n - 1 / 2);
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
/**
 * Generates a type of a given size.
 *
 * @param n The size of the type to generate.
 * @return A generator for types of the specified size.
 *
 * This function is currently used for property tests between MakeTerm and the Menhir parser,
 * so it's not currently set up to generate every possible type.
 */
and gen_typ_sized: int => QCheck.Gen.t(typ) =
  n =>
    QCheck.Gen.(
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
          | 0 => leaf_nodes
          | _ =>
            oneof([
              leaf_nodes,
              {
                let* sizes = gen_non_singleton_array(n);
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
                let+ ident = gen_ident;
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
                let* sizes = gen_non_empty_array(n - 1);
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
              map(x => VarPat(x), gen_ident),
              map(x => StringPat(x), gen_string_literal),
              map(x => BoolPat(x), bool),
              map(
                x => ConstructorPat(x, UnknownType(Internal)),
                gen_constructor_ident,
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
                let* sizes = gen_non_singleton_array(n - 1);
                let+ pats =
                  flatten_a(Array.map((size: int) => self(size), sizes));
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
                ApPat(
                  ConstructorPat(constructor, UnknownType(Internal)),
                  p,
                );
              }, // The parser only handles ApPat with a constructor
              {
                let* p = self((n - 1) / 2);
                let+ t1 = gen_typ_sized((n - 1) / 2);
                CastPat(p, t1, UnknownType(Internal));
              } // The second cast pat isn't present in syntax
            ])
          };
        },
        n,
      )
    );
// TODO Printers, shrinkers stuff
