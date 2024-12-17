open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, qcheck)]
type filter_action =
  | Pause
  | Debug
  | Hide
  | Eval;

[@deriving (show({with_path: false}), sexp, qcheck)]
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

[@deriving (show({with_path: false}), sexp, qcheck)]
type op_bin_bool =
  | And
  | Or;

[@deriving (show({with_path: false}), sexp, qcheck)]
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

[@deriving (show({with_path: false}), sexp, qcheck)]
type op_bin_string =
  | Concat
  | Equals;

// TODO Rename to match others
[@deriving (show({with_path: false}), sexp, qcheck)]
type binOp =
  | IntOp(op_bin_int)
  | FloatOp(op_bin_float)
  | StringOp(op_bin_string)
  | BoolOp(op_bin_bool);

[@deriving (show({with_path: false}), sexp, qcheck)]
type op_un_meta =
  | Unquote;

[@deriving (show({with_path: false}), sexp, qcheck)]
type op_un_int =
  | Minus;

[@deriving (show({with_path: false}), sexp, qcheck)]
type op_un_bool =
  | Not;

[@deriving (show({with_path: false}), sexp, qcheck)]
type op_un =
  | Meta(op_un_meta)
  | Int(op_un_int)
  | Bool(op_un_bool);

[@deriving (show({with_path: false}), sexp, qcheck)]
type typ_provenance =
  | Internal
  | EmptyHole;

[@deriving (show({with_path: false}), sexp, qcheck)]
type tpat =
  | InvalidTPat([@arb small_printable_gen] string)
  | EmptyHoleTPat
  | VarTPat(string);

[@deriving (show({with_path: false}), sexp, qcheck)]
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

[@deriving (show({with_path: false}), sexp, qcheck)]
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
  | InvalidPat(string);

[@deriving (show({with_path: false}), sexp, qcheck)]
type if_consistency =
  | Consistent
  | Inconsistent;

[@deriving (show({with_path: false}), sexp, qcheck)]
type deferral_pos =
  | InAp
  | OutsideAp;

[@deriving (show({with_path: false}), sexp)]
type exp =
  | Int(int)
  | Float(float)
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

let arb_str =
  QCheck.(map(x => String(x), string_small_of(Gen.char_range('a', 'z')))); // Make strings anything other than `"`"

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
          | "Bool" => ident ++ "2"
          | _ => ident
          },
        make(
          ~print=t => t,
          Gen.map2((l, t) => String.make(1, l) ++ t, leading, tail.gen),
        ),
      )
    );

// ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
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
        | "in" => ident ++ "2"
        | _ => ident
        },
      string_gen_of_size(Gen.int_range(1, 5), arb_lower_alpha),
    )
  );

let arb_var = QCheck.(map(x => Var(x), arb_ident));

let rec gen_exp_sized = (n: int): QCheck.Gen.t(exp) =>
  QCheck.(
    let leaf =
      oneof([
        arb_int,
        arb_str,
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
          | 0 => leaf.gen
          | _ =>
            let list_sizes =
              if (n <= 1) {
                // Bug in nat_split for size=0
                Gen.pure([|0, 0, 0, 0, 0|]);
              } else {
                QCheck.Gen.nat_split(
                  ~size=n - 1,
                  5 // Make different size lists
                );
              };

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
                  list_sizes,
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
                  list_sizes,
                ),
              ),
              Gen.map(exp => Test(exp), self(n - 1)),
              Gen.map2(
                (name, typ) => {Constructor(name, typ)},
                arb_constructor_ident.gen,
                gen_typ_sized(n - 1),
              ),
              Gen.map3(
                (op, e1, e2) => BinExp(e1, op, e2),
                gen_binOp,
                self((n - 1) / 2),
                self((n - 1) / 2),
              ),
              Gen.map3(
                (e1, e2, e3) => If(e1, e2, e3),
                self((n - 1) / 3),
                self((n - 1) / 3),
                self((n - 1) / 3),
              ),
            ]);
          }
        },
        n,
      );

    gen
  )
and gen_typ_sized = (n: int): QCheck.Gen.t(typ) => QCheck.Gen.pure(IntType);
// TODO Printers, shrinkers stuff

let gen_exp = QCheck.Gen.sized(gen_exp_sized);
let gen_typ = QCheck.Gen.sized(gen_typ_sized);

let arb_exp = QCheck.make(~print=show_exp, gen_exp);
