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
