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
type typfun_examples =
  | Basic
  | EmptyHole
  | MultiHole /* TODO: Maybe no good examples with Multihole? */
  | Var;

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
  | EqualTrue
  | PolyEqualFalse
  | PolyEqualTrue;

[@deriving (show({with_path: false}), sexp, yojson)]
type example_id =
  | RecTyp
  | Deferral
  | List(list_examples)
  | TypFun(typfun_examples)
  | Fun(fun_examples)
  | Tuple1
  | Tuple2
  | Let(let_examples)
  | TypFunAp
  | FunAp
  | ConAp
  | DeferredAp
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
  | Pipeline1
  | FilterStep
  | FilterEval
  | FilterHide
  | FilterDebug
  | FilterSelector;

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
  | DeferralExp
  | BoolExp
  | IntExp
  | FloatExp
  | StringExp
  | VarExp
  | CtrExp
  | ListExp
  | ConsExp
  | ListConcatExp
  | TypFunctionExp
  | FunctionExp(pat_sub_form_id)
  | TupleExp
  | Tuple2Exp
  | Tuple3Exp
  | LetExp(pat_sub_form_id)
  | TypFunApExp
  | FunApExp
  | ConApExp
  | DeferredApExp
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
  | ForallTyp
  | RecTyp
  | ArrowTyp
  | Arrow3Typ
  | TupleTyp
  | Tuple0Typ
  | Tuple2Typ
  | Tuple3Typ
  | LabelledSumTyp
  | SumTypUnaryConstructorDef
  | SumTypNullaryConstructorDef
  | EmptyHoleTPat
  | MultiHoleTPat
  | VarTPat
  | PipelineExp
  | FilterPause
  | FilterEval
  | FilterDebug
  | FilterHide
  | FilterSelector;

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
  | DeferralExp
  | BoolExp
  | IntExp
  | FloatExp
  | StringExp
  | VarExp
  | CtrExp
  | ListExp
  | ConsExp
  | ListConcatExp
  | TypFunctionExp
  | FunctionExp(pat_sub_form_id)
  | TupleExp
  | Tuple2Exp
  | Tuple3Exp
  | LetExp(pat_sub_form_id)
  | TypFunApExp
  | FunApExp
  | ConApExp
  | DeferredApExp
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
  | ForallTyp
  | RecTyp
  | ArrowTyp
  | Arrow3Typ
  | TupleTyp
  | Tuple0Typ
  | Tuple2Typ
  | Tuple3Typ
  | LabelledSumTyp
  | SumTypUnaryConstructorDef
  | SumTypNullaryConstructorDef
  | EmptyHoleTPat
  | MultiHoleTPat
  | VarTPat
  | FilterPause
  | FilterEval
  | FilterDebug
  | FilterHide
  | FilterSelector;

[@deriving (show({with_path: false}), sexp, yojson)]
type group = {
  id: group_id,
  forms: list(form) // Ordered - more specific to less specific
};

module Simple = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    group_id,
    form_id,
    abstract: (Segment.t, list((Id.t, Id.t))),
    explanation: string,
    examples: list(example),
  };

  let to_group =
      (
        {
          explanation,
          abstract: (syntactic_form, colorings),
          group_id,
          form_id,
          examples,
        }: t,
      ) => (
    explanation,
    colorings,
    {
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
  );

  let mk_1 =
      ((n: string, id: Id.t), mk_form: Piece.t => Segment.t)
      : (Segment.t, list((Id.t, Id.t))) => {
    let p = Example.exp(n);
    (mk_form(p), [(Piece.id(p), id)]);
  };

  let mk_2 =
      (
        (n1: string, id_1: Id.t),
        (n2: string, id_2: Id.t),
        mk_form: (Piece.t, Piece.t) => Segment.t,
      )
      : (Segment.t, list((Id.t, Id.t))) => {
    let (p1, p2) = (Example.exp(n1), Example.exp(n2));
    (mk_form(p1, p2), [(Piece.id(p1), id_1), (Piece.id(p2), id_2)]);
  };

  let mk_3 =
      (
        (n1: string, id_1: Id.t),
        (n2: string, id_2: Id.t),
        (n3: string, id_3: Id.t),
        mk_form: (Piece.t, Piece.t, Piece.t) => Segment.t,
      )
      : (Segment.t, list((Id.t, Id.t))) => {
    let (p1, p2, p3) = (
      Example.exp(n1),
      Example.exp(n2),
      Example.exp(n3),
    );
    (
      mk_form(p1, p2, p3),
      [
        (Piece.id(p1), id_1),
        (Piece.id(p2), id_2),
        (Piece.id(p3), id_3),
      ],
    );
  };
};
