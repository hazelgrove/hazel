open Sexplib.Std;
open Bigint;

module Annotated = {
  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type t('a, 'b) = {
    term: 'a,
    annotation: 'b,
  };
};
let lift = (x): Annotated.t('a, unit) => {
  {
    term: x,
    annotation: (),
  };
};

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
type typ('a) =
  | IntType
  | SIntType
  | StringType
  | FloatType
  | BoolType
  | NatType
  | SumTyp(sumtype('a))
  | UnknownType(typ_provenance)
  | TupleType(list(Annotated.t(typ('a), 'a)))
  | ArrayType(Annotated.t(typ('a), 'a))
  | ArrowType(Annotated.t(typ('a), 'a), Annotated.t(typ('a), 'a))
  | TypVar(string)
  | InvalidTyp(string)
  | ForallType(tpat, Annotated.t(typ('a), 'a))
  | RecType(tpat, Annotated.t(typ('a), 'a))
  | LabelType(string)
  | TupLabelType(Annotated.t(typ('a), 'a), Annotated.t(typ('a), 'a))
  | IndicationTyp(Annotated.t(typ('a), 'a))
and sumterm('a) =
  | Variant(string, option(Annotated.t(typ('a), 'a)))
  | BadEntry(Annotated.t(typ('a), 'a))
and sumtype('a) = list(sumterm('a));
[@deriving (show({with_path: false}), sexp, eq)]
type atom =
  | Int(Bigint.t)
  | SInt(int)
  | Nat(Bigint.t)
  | Float(float)
  | Bool(bool)
  | String(string);
[@deriving (show({with_path: false}), sexp, eq)]
type pat('a) =
  | CastPat(
      Annotated.t(pat('a), 'a),
      Annotated.t(typ('a), 'a),
      Annotated.t(typ('a), 'a),
    )
  | EmptyHolePat
  | WildPat
  | AtomPat(atom)
  | VarPat(string)
  | ConstructorPat(string, option(option(Annotated.t(typ('a), 'a))))
  | TuplePat(list(Annotated.t(pat('a), 'a)))
  | ConsPat(Annotated.t(pat('a), 'a), Annotated.t(pat('a), 'a))
  | ListPat(list(Annotated.t(pat('a), 'a)))
  | ApPat(Annotated.t(pat('a), 'a), Annotated.t(pat('a), 'a))
  | InvalidPat(string) // Menhir parser doesn't actually support invalid pats
  | TupLabelPat(Annotated.t(pat('a), 'a), Annotated.t(pat('a), 'a))
  | LabelPat(string)
  | IndicationPat(Annotated.t(pat('a), 'a));

[@deriving (show({with_path: false}), sexp, qcheck, eq)]
type if_consistency =
  | Consistent
  | Inconsistent;

[@deriving (show({with_path: false}), sexp, qcheck, eq)]
type deferral_pos =
  | InAp
  | OutsideAp;

[@deriving (show({with_path: false}), sexp, eq)]
type exp('a) =
  | Atom(atom)
  | Var(string)
  | Constructor(string, option(option(Annotated.t(typ('a), 'a))))
  | ListExp(list(Annotated.t(exp('a), 'a)))
  | TupleExp(list(Annotated.t(exp('a), 'a)))
  | BinExp(Annotated.t(exp('a), 'a), bin_op, Annotated.t(exp('a), 'a))
  | UnOp(op_un, Annotated.t(exp('a), 'a))
  | Let(
      Annotated.t(pat('a), 'a),
      Annotated.t(exp('a), 'a),
      Annotated.t(exp('a), 'a),
    )
  | Fun(
      Annotated.t(pat('a), 'a),
      Annotated.t(exp('a), 'a),
      option(string),
    )
  | CaseExp(
      Annotated.t(exp('a), 'a),
      list((Annotated.t(pat('a), 'a), Annotated.t(exp('a), 'a))),
    )
  | Label(string)
  | TupLabel(Annotated.t(exp('a), 'a), Annotated.t(exp('a), 'a))
  | Dot(Annotated.t(exp('a), 'a), Annotated.t(exp('a), 'a))
  | ApExp(Annotated.t(exp('a), 'a), Annotated.t(exp('a), 'a))
  | FixF(Annotated.t(pat('a), 'a), Annotated.t(exp('a), 'a))
  | Cast(
      Annotated.t(exp('a), 'a),
      Annotated.t(typ('a), 'a),
      Annotated.t(typ('a), 'a),
    )
  | FailedCast(
      Annotated.t(exp('a), 'a),
      Annotated.t(typ('a), 'a),
      Annotated.t(typ('a), 'a),
    )
  | EmptyHole
  | Filter(
      filter_action,
      Annotated.t(exp('a), 'a),
      Annotated.t(exp('a), 'a),
    )
  | BuiltinFun(string)
  | Undefined
  | Seq(Annotated.t(exp('a), 'a), Annotated.t(exp('a), 'a))
  | Test(Annotated.t(exp('a), 'a))
  | Deferral
  | TypFun(tpat, Annotated.t(exp('a), 'a))
  | Cons(Annotated.t(exp('a), 'a), Annotated.t(exp('a), 'a))
  | ListConcat(Annotated.t(exp('a), 'a), Annotated.t(exp('a), 'a))
  | If(
      Annotated.t(exp('a), 'a),
      Annotated.t(exp('a), 'a),
      Annotated.t(exp('a), 'a),
    )
  | InvalidExp(string)
  | TypAp(Annotated.t(exp('a), 'a), Annotated.t(typ('a), 'a))
  | DynamicErrorHole(Annotated.t(exp('a), 'a), string)
  | TyAlias(tpat, Annotated.t(typ('a), 'a), Annotated.t(exp('a), 'a))
  | Use(Annotated.t(typ('a), 'a), Annotated.t(exp('a), 'a))
  | IndicationExp(Annotated.t(exp('a), 'a));


let rec map_exp_annotation:
  type a b. (a => b, Annotated.t(exp(a), a)) => Annotated.t(exp(b), b) =
  (f, exp) => {
    {
      term:
        switch (exp.term) {
        | Atom(x) => Atom(x)
        | Var(x) => Var(x)
        | Constructor(x, y) =>
          Constructor(x, Option.map(Option.map(map_typ_annotation(f)), y))
        | ListExp(xs) =>
          ListExp(List.map(x => map_exp_annotation(f, x), xs))
        | TupleExp(xs) =>
          TupleExp(List.map(x => map_exp_annotation(f, x), xs))
        | BinExp(x, op, y) =>
          BinExp(map_exp_annotation(f, x), op, map_exp_annotation(f, y))
        | UnOp(op, x) => UnOp(op, map_exp_annotation(f, x))
        | Let(pat, x, body) =>
          Let(
            map_pat_annotation(f, pat),
            map_exp_annotation(f, x),
            map_exp_annotation(f, body),
          )
        | Fun(pat, body, name) =>
          Fun(map_pat_annotation(f, pat), map_exp_annotation(f, body), name)
        | Label(x) => Label(x)
        | TupLabel(x, y) =>
          TupLabel(map_exp_annotation(f, x), map_exp_annotation(f, y))
        | Dot(x, y) =>
          Dot(map_exp_annotation(f, x), map_exp_annotation(f, y))
        | ApExp(x, y) =>
          ApExp(map_exp_annotation(f, x), map_exp_annotation(f, y))
        | CaseExp(x, branches) =>
          CaseExp(
            map_exp_annotation(f, x),
            List.map(
              branch => {
                let (pat, body) = branch;
                (map_pat_annotation(f, pat), map_exp_annotation(f, body));
              },
              branches,
            ),
          )
        | FixF(pat, body) =>
          FixF(map_pat_annotation(f, pat), map_exp_annotation(f, body))
        | Cast(x, typ1, typ2) =>
          Cast(
            map_exp_annotation(f, x),
            map_typ_annotation(f, typ1),
            map_typ_annotation(f, typ2),
          )
        | FailedCast(x, typ1, typ2) =>
          FailedCast(
            map_exp_annotation(f, x),
            map_typ_annotation(f, typ1),
            map_typ_annotation(f, typ2),
          )
        | EmptyHole => EmptyHole
        | Filter(action, x, y) =>
          Filter(action, map_exp_annotation(f, x), map_exp_annotation(f, y))
        | BuiltinFun(x) => BuiltinFun(x)
        | Undefined => Undefined
        | Seq(x, y) =>
          Seq(map_exp_annotation(f, x), map_exp_annotation(f, y))
        | Test(x) => Test(map_exp_annotation(f, x))
        | Deferral => Deferral
        | TypFun(tpat, body) => TypFun(tpat, map_exp_annotation(f, body))
        | Cons(x, y) =>
          Cons(map_exp_annotation(f, x), map_exp_annotation(f, y))
        | ListConcat(x, y) =>
          ListConcat(map_exp_annotation(f, x), map_exp_annotation(f, y))
        | If(cond, then_branch, else_branch) =>
          If(
            map_exp_annotation(f, cond),
            map_exp_annotation(f, then_branch),
            map_exp_annotation(f, else_branch),
          )
        | InvalidExp(x) => InvalidExp(x)
        | TypAp(x, typ) =>
          TypAp(map_exp_annotation(f, x), map_typ_annotation(f, typ))
        | DynamicErrorHole(x, msg) =>
          DynamicErrorHole(map_exp_annotation(f, x), msg)
        | TyAlias(tpat, typ, body) =>
          TyAlias(
            tpat,
            map_typ_annotation(f, typ),
            map_exp_annotation(f, body),
          )
        | Use(typ, x) =>
          Use(map_typ_annotation(f, typ), map_exp_annotation(f, x))
        | IndicationExp(x) => IndicationExp(map_exp_annotation(f, x))
        },
      annotation: f(exp.annotation),
    };
  }

and map_pat_annotation:
  type a b. (a => b, Annotated.t(pat(a), a)) => Annotated.t(pat(b), b) =
  (f, pat) => {
    {
      term:
        switch (pat.term) {
        | EmptyHolePat => EmptyHolePat
        | WildPat => WildPat
        | AtomPat(x) => AtomPat(x)
        | VarPat(x) => VarPat(x)
        | ConstructorPat(x, y) =>
          ConstructorPat(
            x,
            Option.map(Option.map(map_typ_annotation(f)), y),
          )
        | TuplePat(xs) =>
          TuplePat(List.map(x => map_pat_annotation(f, x), xs))
        | ConsPat(x, y) =>
          ConsPat(map_pat_annotation(f, x), map_pat_annotation(f, y))
        | ListPat(xs) =>
          ListPat(List.map(x => map_pat_annotation(f, x), xs))
        | ApPat(x, y) =>
          ApPat(map_pat_annotation(f, x), map_pat_annotation(f, y))
        | InvalidPat(x) => InvalidPat(x)
        | TupLabelPat(x, y) =>
          TupLabelPat(map_pat_annotation(f, x), map_pat_annotation(f, y))
        | LabelPat(x) => LabelPat(x)
        | IndicationPat(x) => IndicationPat(map_pat_annotation(f, x))
        | CastPat(pat, typ1, typ2) =>
          CastPat(
            map_pat_annotation(f, pat),
            map_typ_annotation(f, typ1),
            map_typ_annotation(f, typ2),
          )
        },
      annotation: f(pat.annotation),
    };
  }

and map_typ_annotation:
  type a b. (a => b, Annotated.t(typ(a), a)) => Annotated.t(typ(b), b) =
  (f, typ) => {
    {
      term:
        switch (typ.term) {
        | IntType => IntType
        | SIntType => SIntType
        | StringType => StringType
        | FloatType => FloatType
        | BoolType => BoolType
        | NatType => NatType
        | SumTyp(x) =>
          SumTyp(
            List.map(
              sumterm =>
                switch (sumterm) {
                | Variant(name, None) => Variant(name, None)
                | Variant(name, Some(annot)) =>
                  Variant(name, Some(map_typ_annotation(f, annot)))
                | BadEntry(annot) => BadEntry(map_typ_annotation(f, annot))
                },
              x,
            ),
          )
        | UnknownType(x) => UnknownType(x)
        | TupleType(xs) =>
          TupleType(List.map(x => map_typ_annotation(f, x), xs))
        | ArrayType(x) => ArrayType(map_typ_annotation(f, x))
        | ArrowType(x, y) =>
          ArrowType(map_typ_annotation(f, x), map_typ_annotation(f, y))
        | TypVar(x) => TypVar(x)
        | InvalidTyp(x) => InvalidTyp(x)
        | ForallType(tpat, typ) =>
          ForallType(tpat, map_typ_annotation(f, typ))
        | RecType(tpat, typ) => RecType(tpat, map_typ_annotation(f, typ))
        | LabelType(x) => LabelType(x)
        | TupLabelType(x, y) =>
          TupLabelType(map_typ_annotation(f, x), map_typ_annotation(f, y))
        | IndicationTyp(x) => IndicationTyp(map_typ_annotation(f, x))
        },
      annotation: f(typ.annotation),
    };
  };
