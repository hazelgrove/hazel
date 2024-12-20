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

// TODO Rename to match others
[@deriving (show({with_path: false}), sexp, qcheck, eq)]
type binOp =
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

[@deriving (show({with_path: false}), sexp, qcheck, eq)]
type tpat =
  | InvalidTPat([@arb small_printable_gen] string)
  | EmptyHoleTPat
  | VarTPat(string);

[@deriving (show({with_path: false}), sexp, qcheck, eq)]
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

[@deriving (show({with_path: false}), sexp, qcheck, eq)]
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
  | InvalidPat(string); // TODO Menhir parser doesn't actually support invalid pats

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
  | BinExp(exp, binOp, exp)
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

let arb_int = QCheck.(map(x => Int(x), small_int));
let arb_str = QCheck.(string_small_of(Gen.char_range('a', 'z')));
let arb_str_exp = QCheck.map(x => String(x), arb_str); // Make strings anything other than `"`"

// Floats are positive because we use UnOp minus
let arb_float = QCheck.(map(x => Float(x), pos_float));
let arb_lower_alpha = QCheck.Gen.char_range('a', 'z');

let arb_constructor_ident =
  QCheck.
    // TODO handle full constructor ident including nums
    (
      let leading = Gen.char_range('A', 'Z');
      let tail = string_gen_of_size(Gen.int_range(1, 4), arb_lower_alpha);
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
      string_gen_of_size(Gen.int_range(1, 1), arb_lower_alpha),
    )
  );

let sized_arr = (n: int) =>
  QCheck.Gen.(
    let list_size = n <= 1 ? pure(0) : int_range(2, n);

    list_size
    >>= (
      x =>
        switch (x) {
        | 0 => pure([||])
        | _ => nat_split(~size=x, n - x)
        }
    )
  );

let non_single_element_arr = (n: int) =>
  QCheck.Gen.(
    let list_size =
      frequency([(1, pure(0)), (n, n <= 1 ? pure(0) : int_range(2, n))]);

    list_size
    >>= (
      x =>
        switch (x) {
        | 0 => pure([||])
        | _ => nat_split(~size=x, n - x)
        }
    )
  );
let arb_var = QCheck.(map(x => Var(x), arb_ident));

let rec gen_exp_sized = (n: int): QCheck.Gen.t(exp) =>
  QCheck.(
    let leaf =
      oneof([
        arb_int,
        arb_str_exp,
        arb_float,
        arb_var,
        always(~print=show_exp, EmptyHole),
        always(~print=show_exp, TupleExp([])),
        always(~print=show_exp, ListExp([])),
      ]);

    let gen: Gen.t(exp) =
      QCheck.Gen.fix(
        (self: int => Gen.t(exp), n) => {
          switch (n) {
          | 0
          | 1 => leaf.gen
          | _ =>
            Gen.oneof([
              leaf.gen,
              Gen.join(
                Gen.map(
                  (sizes: array(int)) => {
                    let exps = Array.map((size: int) => self(size), sizes);
                    let flattened = Gen.flatten_a(exps);
                    Gen.map(
                      (exps: array(exp)) => ListExp(Array.to_list(exps)),
                      flattened,
                    );
                  },
                  sized_arr(n),
                ),
              ),
              Gen.join(
                Gen.map(
                  (sizes: array(int)) => {
                    let exps = Array.map((size: int) => self(size), sizes);
                    let flattened = Gen.flatten_a(exps);
                    Gen.map(
                      (exps: array(exp)) => TupleExp(Array.to_list(exps)), // TODO See if there's a way to do unit and various sizes here
                      flattened,
                    );
                  },
                  non_single_element_arr(n),
                ),
              ),
              Gen.map(exp => Test(exp), self(n - 1)),
              Gen.map(
                name => Constructor(name, UnknownType(Internal)),
                arb_constructor_ident.gen,
              ),
              // TODO Ignoring typ because we don't serialize those
              // Gen.map2(
              //   (name, typ) => {Constructor(name, typ)},
              //   arb_constructor_ident.gen,
              //   gen_typ_sized(n - 1),
              // ),
              Gen.map3(
                (op, e1, e2) => BinExp(e1, op, e2),
                gen_binOp,
                self((n - 1) / 2),
                self((n - 1) / 2),
              ),
              // Gen.map2((op, e) => UnOp(op, e), gen_op_un, self(n - 1)), // TODO ExpToSegment broken for UnOp
              Gen.map3(
                (e1, e2, e3) => If(e1, e2, e3),
                self((n - 1) / 3),
                self((n - 1) / 3),
                self((n - 1) / 3),
              ),
              Gen.map3(
                (p, e1, e2) => Let(p, e1, e2),
                gen_pat_sized((n - 1) / 3),
                self((n - 1) / 3),
                self((n - 1) / 3),
              ),
            ])
          }
        },
        n,
      );

    gen
  )
and gen_typ_sized: int => QCheck.Gen.t(typ) =
  n =>
    /*
       | InvalidTyp(string)
     and sumterm =
       | Variant(string, option(typ))
       | BadEntry(typ)
       */
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
              join(
                map(
                  (sizes: array(int)) => {
                    let typs = Array.map((size: int) => self(size), sizes);

                    let flattened = flatten_a(typs);
                    map(
                      (typs: array(typ)) => TupleType(Array.to_list(typs)),
                      flattened,
                    );
                  },
                  non_single_element_arr(n),
                ),
              ),
              map(t => ArrayType(t), self(n - 1)),
              map2(
                (t1, t2) => ArrowType(t1, t2),
                self((n - 1) / 2),
                self((n - 1) / 2),
              ),
              map(x => TypVar(x), arb_ident.gen),
              map2(
                (tpat, t) => ForallType(tpat, t),
                arb_tpat.gen,
                self(n - 1),
              ),
              map2(
                (tpat, t) => RecType(tpat, t),
                arb_tpat.gen,
                self(n - 1),
              ),
              join(
                map(
                  sizes => {
                    let sumterms =
                      Array.map(
                        (size: int) => {
                          let optional_typ = option(self(size));
                          let constructor = arb_constructor_ident.gen;
                          let variant =
                            map2(
                              (name, typ) => Variant(name, typ),
                              constructor,
                              optional_typ,
                            );
                          frequency([
                            (5, variant),
                            (1, return(BadEntry(UnknownType(EmptyHole)))),
                          ]);
                        },
                        sizes,
                      );

                    let flattened: t(array(sumterm)) = flatten_a(sumterms);

                    map(x => SumTyp(Array.to_list(x)), flattened);
                  },
                  sized_arr(n - 1),
                ),
              ),
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
              map(x => StringPat(x), arb_str.gen),
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
              map2(
                (p1, p2) => ConsPat(p1, p2),
                self((n - 1) / 2),
                self((n - 1) / 2),
              ),
              join(
                map(
                  sizes => {
                    let pats = Array.map((size: int) => self(size), sizes);

                    let flattened = flatten_a(pats);

                    map(x => TuplePat(Array.to_list(x)), flattened);
                  },
                  non_single_element_arr(n - 1),
                ),
              ),
              join(
                map(
                  sizes => {
                    let pats = Array.map((size: int) => self(size), sizes);

                    let flattened = flatten_a(pats);

                    map(x => ListPat(Array.to_list(x)), flattened);
                  },
                  sized_arr(n - 1),
                ),
              ),
              map2(
                (p1, p2) => ApPat(p1, p2),
                self((n - 1) / 2),
                self((n - 1) / 2),
              ),
              map3(
                (p, t1, t2) => CastPat(p, t1, t2),
                self((n - 1) / 3),
                gen_typ_sized((n - 1) / 3),
                gen_typ_sized((n - 1) / 3),
              ),
            ])
          };
        },
        n,
      )
    );
// TODO Printers, shrinkers stuff

let gen_exp = QCheck.Gen.sized(gen_exp_sized);
let gen_typ = QCheck.Gen.sized(gen_typ_sized);

let arb_exp = QCheck.make(~print=show_exp, gen_exp);
