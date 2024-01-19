open Sexplib.Std;
open Haz3lcore;

// TODO Make unified way of using consistent metavariables for syntactic forms
// TODO Use /tau instead of ty when can do that and still have highlighting work

[@deriving (show({with_path: false}), sexp, yojson)]
type list_examples =
  | Int
  | Tuple
  | Cons1
  | Cons2;

[@deriving (show({with_path: false}), sexp, yojson)]
type fun_examples =
  | Basic
  | Wild
  | IntLit
  | FloatLit
  | BoolLit
  | StrLit
  | Triv
  | ListNil
  | ListLit
  | ConsHd
  | ConsSnd
  | VarIncr
  | VarAnd
  | Tuple2
  | Tuple3
  | Ctr
  | Ap;

[@deriving (show({with_path: false}), sexp, yojson)]
type let_examples =
  | Basic
  | Wild
  | IntLit
  | FloatLit
  | BoolLit
  | StrLit
  | Triv
  | ListNil
  | ListLit
  | ConsHd
  | ConsSnd
  | Var
  | Tuple2
  | Tuple3
  | Ctr
  | Ap;

[@deriving (show({with_path: false}), sexp, yojson)]
type numeric_bin_op_examples =
  | Plus
  | Minus
  | Times
  | Power
  | Divide
  | LessThanTrue
  | LessThanFalse
  | LessThanEqualLess
  | LessThanEqualEqual
  | LessThanEqualFalse
  | GreaterThanTrue
  | GreaterThanFalse
  | GreaterThanEqualGreater
  | GreaterThanEqualEqual
  | GreaterThanEqualFalse
  | EqualFalse
  | EqualTrue;

[@deriving (show({with_path: false}), sexp, yojson)]
type example_id =
  | List(list_examples)
  | Fun(fun_examples)
  | Tuple1
  | Tuple2
  | Let(let_examples)
  | FunAp
  | ConAp
  | IfTrue
  | IfFalse
  | SeqBasic
  | SeqTest
  | TestTrue
  | TestFalse
  | IntUnaryMinus
  | Int(numeric_bin_op_examples)
  | Float(numeric_bin_op_examples)
  | AndFalse
  | AndTrue
  | OrFalse
  | OrTrue
  | StringEqualFalse
  | StringEqualTrue
  | CaseWildSimple
  | CaseWildTuple
  | CaseInt
  | CaseBool
  | Pipeline1;

[@deriving (show({with_path: false}), sexp, yojson)]
type example = {
  sub_id: example_id,
  term: Segment.t,
  message: string,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type pat_sub_form_id =
  | Base
  | EmptyHole
  | MultiHole
  | Wild
  | Int
  | Float
  | Bool
  | String
  | Triv
  | ListNil
  | ListLit
  | ListCons
  | Var
  | Tuple
  | Tuple2
  | Tuple3
  | Ctr
  | Ap;

[@deriving (show({with_path: false}), sexp, yojson)]
type form_id =
  | EmptyHoleExp
  | MultiHoleExp
  | TrivExp
  | BoolExp
  | IntExp
  | FloatExp
  | StringExp
  | VarExp
  | CtrExp
  | ListExp
  | ConsExp
  | ListConcatExp
  | FunctionExp(pat_sub_form_id)
  | TupleExp
  | Tuple2Exp
  | Tuple3Exp
  | LetExp(pat_sub_form_id)
  | FunApExp
  | ConApExp
  | IfExp
  | SeqExp
  | TestExp
  | UnOpExp(Term.UExp.op_un)
  | BinOpExp(Term.UExp.op_bin)
  | CaseExp
  | TyAliasExp
  | EmptyHolePat
  | MultiHolePat
  | WildPat
  | IntPat
  | FloatPat
  | BoolPat
  | StrPat
  | TrivPat
  | VarPat
  | CtrPat
  | ListLitPat
  | ListNilPat
  | ConsPat
  | Cons2Pat
  | TuplePat
  | Tuple2Pat
  | Tuple3Pat
  | ApPat
  | TypAnnPat
  | EmptyHoleTyp
  | MultiHoleTyp
  | IntTyp
  | FloatTyp
  | BoolTyp
  | StrTyp
  | VarTyp
  | ListTyp
  | ArrowTyp
  | Arrow3Typ
  | TupleTyp
  | Tuple2Typ
  | Tuple3Typ
  | LabelledSumTyp
  | SumTypUnaryConstructorDef
  | SumTypNullaryConstructorDef
  | EmptyHoleTPat
  | MultiHoleTPat
  | VarTPat
  | PipelineExp;

[@deriving (show({with_path: false}), sexp, yojson)]
type form = {
  id: form_id,
  syntactic_form: Segment.t,
  expandable_id: option((Id.t, Segment.t)),
  explanation: string,
  examples: list(example),
};

// HANNAH - TODO: Not sure this should be different from form_id - maybe just one id
// MAYBE don't even need an id at all for the group - just use the most specific (1st) form id in forms
[@deriving (show({with_path: false}), sexp, yojson)]
type group_id =
  | EmptyHoleExp
  | MultiHoleExp
  | TrivExp
  | BoolExp
  | IntExp
  | FloatExp
  | StringExp
  | VarExp
  | CtrExp
  | ListExp
  | ConsExp
  | ListConcatExp
  | FunctionExp(pat_sub_form_id)
  | TupleExp
  | Tuple2Exp
  | Tuple3Exp
  | LetExp(pat_sub_form_id)
  | FunApExp
  | ConApExp
  | IfExp
  | SeqExp
  | TestExp
  | UnOpExp(Term.UExp.op_un)
  | BinOpExp(Term.UExp.op_bin)
  | CaseExp
  | TyAliasExp
  | PipelineExp
  | EmptyHolePat
  | MultiHolePat
  | WildPat
  | IntPat
  | FloatPat
  | BoolPat
  | StrPat
  | TrivPat
  | VarPat
  | CtrPat
  | ListLitPat
  | ListNilPat
  | ConsPat
  | Cons2Pat
  | TuplePat
  | Tuple2Pat
  | Tuple3Pat
  | ApPat
  | TypAnnPat
  | EmptyHoleTyp
  | MultiHoleTyp
  | IntTyp
  | FloatTyp
  | BoolTyp
  | StrTyp
  | VarTyp
  | ListTyp
  | ArrowTyp
  | Arrow3Typ
  | TupleTyp
  | Tuple2Typ
  | Tuple3Typ
  | LabelledSumTyp
  | SumTypUnaryConstructorDef
  | SumTypNullaryConstructorDef
  | EmptyHoleTPat
  | MultiHoleTPat
  | VarTPat;

[@deriving (show({with_path: false}), sexp, yojson)]
type group = {
  id: group_id,
  forms: list(form) // Ordered - more specific to less specific
};

[@deriving (show({with_path: false}), sexp, yojson)]
type simple = {
  group,
  explanation: string,
  colorings: list((Id.t, Id.t)),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type single_doc = {
  explanation: string,
  colorings: list((Id.t, Id.t)),
  group_id,
  form_id,
  syntactic_form: Segment.t,
  examples: list(example),
};

let single_to_group =
    (
      {explanation, colorings, group_id, form_id, syntactic_form, examples}: single_doc,
    )
    : simple => {
  explanation,
  colorings,
  group: {
    id: group_id,
    forms: [
      {
        id: form_id,
        syntactic_form,
        expandable_id: None,
        explanation: "",
        examples,
      },
    ],
  },
};
