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
  | SumTyp(typ, option(typ))
  | SumTerm(string, option(typ))
  | UnknownType(typ_provenance)
  | TupleType(list(typ))
  | ArrayType(typ)
  | ArrowType(typ, typ)
  | TypVar(string)
  | InvalidTyp(string)
  | ForallType(tpat, typ)
  | RecType(tpat, typ);

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

// ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
// Can't be t, e, tp, or p because of the lexer
let arb_ident =
  QCheck.(
    let arb_alpha = Gen.char_range('a', 'd'); // TODO make this support full indent instead of just lower alpha
    string_gen_of_size(Gen.int_range(1, 5), arb_alpha)
  );

let arb_var = QCheck.(map(x => Var(x), arb_ident));

let arb_exp_sized = (size: int): QCheck.arbitrary(exp) => {
  open QCheck;
  let i = QCheck.small_int;
  let foo = arb_typ_sized;
  oneof([arb_int, arb_str, arb_float, arb_var]);
};
