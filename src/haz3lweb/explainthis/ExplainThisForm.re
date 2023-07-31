open Sexplib.Std;
open Haz3lcore;

// TODO Make unified way of using consistent metavariables for syntactic forms
// TODO Use /tau instead of ty when can do that and still have highlighting work

[@deriving (show({with_path: false}), sexp, yojson)]
type example_id =
  | IntList
  | TupleList
  | Cons1
  | Cons2
  | BasicFun
  | WildFun
  | IntLitFun
  | FloatLitFun
  | BoolLitFun
  | StrLitFun
  | TrivFun
  | ListNilFun
  | ListListFun
  | ConsHdFun
  | ConsSndFun
  | VarIncrFun
  | VarAndFun
  | Tuple2Fun
  | Tuple3Fun
  | TagFun
  | ApFun
  | Tuple1
  | Tuple2
  | BasicLet
  | WildLet
  | IntLet
  | FloatLet
  | BoolLet
  | StrLet
  | TrivLet
  | ListLitLet
  | ListNilLet
  | ConsHdLet
  | ConsSndLet
  | VarLet
  | Tuple2Let
  | Tuple3Let
  | TagLet
  | ApLet
  | FunAp
  | ConAp
  | IfTrue
  | IfFalse
  | SeqBasic
  | SeqTest
  | TestTrue
  | TestFalse
  | IntUnaryMinus
  | IntPlus
  | IntMinus
  | IntTimes
  | IntPower
  | IntDivide
  | IntLessThanTrue
  | IntLessThanFalse
  | IntLessThanEqualLess
  | IntLessThanEqualEqual
  | IntLessThanEqualFalse
  | IntGreaterThanTrue
  | IntGreaterThanFalse
  | IntGreaterThanEqualGreater
  | IntGreaterThanEqualEqual
  | IntGreaterThanEqualFalse
  | IntEqualFalse
  | IntEqualTrue
  | FloatPlus
  | FloatMinus
  | FloatTimes
  | FloatPower
  | FloatDivide
  | FloatLessThanTrue
  | FloatLessThanFalse
  | FloatLessThanEqualLess
  | FloatLessThanEqualEqual
  | FloatLessThanEqualFalse
  | FloatGreaterThanTrue
  | FloatGreaterThanFalse
  | FloatGreaterThanEqualGreater
  | FloatGreaterThanEqualEqual
  | FloatGreaterThanEqualFalse
  | FloatEqualFalse
  | FloatEqualTrue
  | AndFalse
  | AndTrue
  | OrFalse
  | OrTrue
  | StringEqualFalse
  | StringEqualTrue
  | CaseWildSimple
  | CaseWildTuple
  | CaseInt
  | CaseBool;

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
  | Tag
  | Ap;

[@deriving (show({with_path: false}), sexp, yojson)]
type un_op_sub_form_id =
  | IntMinus;

[@deriving (show({with_path: false}), sexp, yojson)]
type bin_op_sub_form_id =
  | IntPlus
  | IntMinus
  | IntTimes
  | IntPower
  | IntDivide
  | IntLessThan
  | IntLessThanEqual
  | IntGreaterThan
  | IntGreaterThanEqual
  | IntEqual
  | FloatPlus
  | FloatMinus
  | FloatTimes
  | FloatPower
  | FloatDivide
  | FloatLessThan
  | FloatLessThanEqual
  | FloatGreaterThan
  | FloatGreaterThanEqual
  | FloatEqual
  | And
  | Or
  | StringEqual;

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
  | TagExp
  | ListExp
  | ConsExp
  | FunctionExp(pat_sub_form_id)
  | Tuple
  | Tuple2
  | Tuple3
  | Let(pat_sub_form_id)
  | FunAp
  | ConAp
  | If
  | Seq
  | Test
  | UnOp(un_op_sub_form_id)
  | BinOp(bin_op_sub_form_id)
  | Case;

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
  | TagExp
  | ListExp
  | ConsExp
  | FunctionExp(pat_sub_form_id)
  | Tuple
  | Tuple2
  | Tuple3
  | Let(pat_sub_form_id)
  | FunAp
  | ConAp
  | If
  | Seq
  | Test
  | UnOp(un_op_sub_form_id)
  | BinOp(bin_op_sub_form_id)
  | Case;

[@deriving (show({with_path: false}), sexp, yojson)]
type group = {
  id: group_id,
  forms: list(form) // Ordered - more specific to less specific
};
