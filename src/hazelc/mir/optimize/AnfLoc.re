open Sexplib.Std;

module StmtLoc = {
  [@deriving sexp]
  type t = list(int);

  let init = [0];

  let next =
    fun
    | [i, ...is] => [i + 1, ...is]
    | [] => failwith("unreachable nil list");

  let nest = is => [0, ...is];

  let compare = List.compare(Int.compare);
};

[@deriving sexp]
type prog =
  | PBody(int, stmt)
  | PReturn

[@deriving sexp]
and stmt =
  | SPat(pat)
  | SComp(comp)

[@deriving sexp]
and comp =
  | CImm(imm)
  | CBinOp(bin_op)
  | CAp(ap)
  | CFun(fun_)
  | CCons(cons)
  | CPair(pair)
  | CInj(imm)
  | CCase(case)

[@deriving sexp]
and bin_op =
  | BinLeft(imm)
  | BinRight(imm)

[@deriving sexp]
and ap =
  | ApFun(imm)
  | ApArg(imm)

[@deriving sexp]
and fun_ =
  | FunParam(pat)
  | FunBody(prog)

[@deriving sexp]
and cons =
  | ConsHead(imm)
  | ConsTail(imm)

[@deriving sexp]
and pair =
  | PairLeft(imm)
  | PairRight(imm)

[@deriving sexp]
and case =
  | CaseScrut(imm)
  | CaseRules(int, rule)

[@deriving sexp]
and rule =
  | RulePat(pat)
  | RuleBody(prog)

[@deriving sexp]
and imm = unit

[@deriving sexp]
and pat =
  | PHere
  | PInj(pat)
  | PCons(pat_cons)
  | PPair(pat_pair)

[@deriving sexp]
and pat_cons =
  | PConsLeft(pat)
  | PConsRight(pat)

[@deriving sexp]
and pat_pair =
  | PPairLeft(pat)
  | PPairRight(pat);
