open Util;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type exp_term(
  'exp,
  'typ,
  'pat,
  'deferral_position_t,
  'tpat,
  'closure_environment,
) =
  | Invalid(string)
  | EmptyHole
  | MultiHole(list('exp))
  | DynamicErrorHole('exp, InvalidOperationError.t)
  | Deferral('deferral_position_t)
  | Undefined
  | Atom(Atom.t)
  | ListLit(list('exp))
  /* The type double-option field of this constructor is required to assign the correct
     statics to constructors after evaluation. In dynamic expressions `Some(None)` means
     that it is a free constructor, while Some(Some(t)) means it has type t. In user expressions
     this field is None.*/
  | Constructor(string, option(option('typ)))
  | Fun('pat, 'exp, option('typ), option(Var.t)) // typ_t field is only used to display types in results
  | TypFun('tpat, 'exp, option(Var.t))
  | Tuple(list('exp))
  | Label(string)
  | ExplicitNonlabel
  | TupLabel('exp, 'exp)
  | Dot('exp, 'exp)
  | LivelitName(string)
  | Var(Var.t)
  | Let('pat, 'exp, 'exp)
  | FixF('pat, 'exp, option('closure_environment))
  | TyAlias('tpat, 'typ, 'exp)
  | Use('typ, 'exp)
  | Ap(Operators.ap_direction, 'exp, 'exp)
  | TypAp('exp, 'typ)
  | DeferredAp('exp, list('exp))
  | If('exp, 'exp, 'exp)
  | Seq('exp, 'exp)
  | Test('exp)
  | HintedTest('exp, 'exp)
  | Filter('exp, 'exp)
  | Closure([@show.opaque] 'closure_environment, 'exp)
  | Parens('exp) // (
  | Probe('exp, Probe.t)
  | Cons('exp, 'exp)
  | ListConcat('exp, 'exp)
  | UnOp(Operators.op_un, 'exp)
  | BinOp(Operators.op_bin, 'exp, 'exp)
  | BuiltinFun(string)
  | Match('exp, list(('pat, 'exp)))
  | TupleExtension('exp, 'exp)
  | Asc('exp, 'typ);
