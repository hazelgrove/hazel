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
  | SIntLit
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
  | TupLabel
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
  | TupLabel
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
  | GreaterThanEqualFalse;

[@deriving (show({with_path: false}), sexp, yojson)]
type example_id =
  | RecTyp
  | Deferral
  | List(list_examples)
  | TypFun(typfun_examples)
  | Fun(fun_examples)
  | Label1
  | Label2
  | Label3
  | Dot1
  | Dot2
  | DotTyp
  | Fix2
  | Tuple1
  | Tuple2
  | TupleLabeled1
  | TupleLabeled2
  | TupleLabeled3
  | TupleExtension1
  | TupleExtension2
  | TupleExtension3
  | Let(let_examples)
  | Theorem
  | ProofOf
  | Forall
  | Yes
  | UseExp1
  | TypFunAp
  | FunAp
  | ConAp
  | LivelitAp
  | DeferredAp
  | IfTrue
  | IfFalse
  | SeqBasic
  | SeqTest
  | TestTrue
  | TestFalse
  | HintedTestTrue
  | HintedTestFalse
  | IntUnaryMinus
  | Int(numeric_bin_op_examples)
  | Float(numeric_bin_op_examples)
  | FloatEqualFalse
  | FloatEqualTrue
  | PolyEqualFalse
  | PolyEqualTrue
  | PolyNotEqualTrue
  | PolyNotEqualFalse
  | AndFalse
  | AndTrue
  | OrFalse
  | OrTrue
  | CaseWildSimple
  | CaseWildTuple
  | CaseInt
  | CaseBool
  | VoidAbsurd
  | Pipeline1
  | FilterStep
  | FilterEval
  | FilterHide
  | FilterDebug
  | FilterSelector
  | Undefined1
  | Undefined2
  | Asc1
  | Asc2
  | Asc3
  | Module1
  | ModLet1
  | ModType1
  | Sig1
  | SigLet1
  | SigType1
  | ModuleKeyword1
  | ModuleKeywordDecl1
  | SigTypeAbstract1;

/* No deriving: nothing serializes an `example`, `form` or `group`. Only the id
   enums need serializers, because `ExplainThisModel.t` is what `Store`
   persists and it holds ids alone. Deriving these too would force `Segment.t`
   through sexp/yojson for no consumer. */
type example = {
  sub_id: example_id,
  term: Segment.t,
  message: string,
};

[@deriving (show({with_path: false}), sexp, yojson, enumerate)]
type pat_sub_form_id =
  | Base
  | EmptyHole
  | MultiHole
  | Wild
  | SInt
  | Int
  | Float
  | Bool
  | String
  | Triv
  | ListNil
  | ListLit
  | ListCons
  | Var
  | TupLabel
  | Tuple
  | Tuple2
  | Tuple3
  | Ctr
  | ApFunc
  | ApCons;

/* `enumerate` gives all_of_form_id, which the characterization test uses to
   tell "this doc is not covered by the corpus" apart from "this doc cannot be
   reached at all". */
[@deriving (show({with_path: false}), sexp, yojson, enumerate)]
type form_id =
  | Derivation
  | EmptyHoleExp
  | MultiHoleExp
  | TrivExp
  | UndefinedExp
  | DeferralExp
  | BoolExp
  | IntExp
  | SIntExp
  | NatExp
  | FloatExp
  | StringExp
  | VarExp
  | CtrExp
  | ListExp
  | ConsExp
  | ListConcatExp
  | TypFunctionExp
  | FunctionExp(pat_sub_form_id)
  | LabeledExp
  | DotExp
  | TupleExp
  | Tuple2Exp
  | Tuple3Exp
  | LetExp(pat_sub_form_id)
  | FixExp(pat_sub_form_id)
  | TheoremExp
  | ProofObjectExp
  | TypFunApExp
  | FunApExp
  | ConApExp
  | DeferredApExp
  | LivelitApExp
  | LivelitName
  | IfExp
  | SeqExp
  | UseExp
  | TestExp
  | HintedTestExp
  | UnOpExp(Language.Operators.op_un)
  | BinOpExp(Language.Operators.op_bin)
  | CaseExp
  | TyAliasExp
  | EmptyHolePat
  | MultiHolePat
  | WildPat
  | IntPat
  | SIntPat
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
  | LabeledPat
  | TuplePat
  | Tuple2Pat
  | Tuple3Pat
  | ApFuncPat
  | ApConsPat
  | TypAnnPat
  | EmptyHoleTyp
  | MultiHoleTyp
  | IntTyp
  | SIntTyp
  | NatTyp
  | FloatTyp
  | BoolTyp
  | StrTyp
  | VoidTyp
  | VarTyp
  | ListTyp
  | PolyTyp
  | RecTyp
  | ArrowTyp
  | Arrow3Typ
  | LabeledTyp
  | TupleTyp
  | Tuple0Typ
  | Tuple2Typ
  | Tuple3Typ
  | DotTyp
  | Label
  | ForallExp
  | ProofOfTyp
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
  | FilterSelector
  | AscExp
  | TupleExtensionExp
  | ModuleExp
  | ModLetDecl
  | ModTypeDecl
  | SigTyp
  | SigLetDecl
  | SigTypeDecl
  | ModuleKeywordExp
  | ModuleKeywordDecl
  | SigTypeAbstractDecl;

type form = {
  id: form_id,
  syntactic_form: Segment.t,
  /* Pairs a placeholder piece of `syntactic_form` with the user-code term it
     stands for, so the two can be highlighted in the same colour. Belongs to
     the form rather than to the caller: which mapping applies depends on which
     form of a group is being shown, and only the form knows its own pieces. */
  colorings: list((Id.t, Id.t)),
  expandable_id: option((Id.t, Segment.t)),
  explanation: string,
  examples: list(example),
};

/* A group's id is the id of its most specific form, so one enum serves both.
   Kept as a named alias so signatures still say which of the two they mean. */
[@deriving (show({with_path: false}), sexp, yojson, enumerate)]
type group_id = form_id;

type group = {
  id: group_id,
  forms: list(form) // Ordered - more specific to less specific
};

/* A group offering a single form. Its id comes from the form, so the two cannot
   disagree. */
let singleton = (form: form): group => {
  id: form.id,
  forms: [form],
};

module Simple = {
  type t = {
    group_id,
    form_id,
    abstract: (Segment.t, list((Id.t, Id.t))),
    explanation: string,
    examples: list(example),
  };

  /* The form carries its own explanation and colorings, so this is a plain
     group with nothing for callers to thread alongside it. */
  let to_group =
      (
        {
          explanation,
          abstract: (syntactic_form, colorings),
          group_id,
          form_id,
          examples,
        }: t,
      )
      : group => {
    id: group_id,
    forms: [
      {
        id: form_id,
        syntactic_form,
        colorings,
        expandable_id: None,
        explanation,
        examples,
      },
    ],
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
};
