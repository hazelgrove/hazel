/* open Sexplib.Std; */
/* open AnfLoc; */

/* module StmtLocMap = MapSexp.Make(StmtLoc); */

/* module Facts = { */
/* [@deriving sexp] */
/* type alias = { */
/* loc: StmtLoc.t, */
/* name: Var.t, */
/* }; */

/* [@deriving sexp] */
/* type fun_ = { */
/* loc: StmtLoc.t, */
/* ret_name: option(Var.t), */
/* }; */

/* [@deriving sexp] */
/* type tracked = */
/* | [>* Tracks to a function definition. <] */
/* TFunDef(fun_) */
/* | [>* Tracks to the return of named function. <] */
/* TFunReturn(Var.t) */
/* | [>* Tracks to the parameter of the enclosing function. <] */
/* TFunParam */
/* | [>* Tracks to potential aliases. <] */
/* TAliases(list(alias)); */

/* [@deriving sexp] */
/* type bind = */
/* | Searching */
/* | Tracked(tracked); */

/* [@deriving sexp] */
/* type binds = VarMap.t_(bind); */

/* [@deriving sexp] */
/* type calls = StmtLocMap.t((Anf.completeness, Var.t)); */

/* [@deriving sexp]; */
/* type t = { */
/* binds, */
/* calls, */
/* }; */

/* let init = {binds: VarMap.empty, calls: StmtLocMap.empty}; */

/* let lookup_binds = facts => VarMap.lookup(facts.binds); */

/* let extend_binds = (facts, assoc) => { */
/* ...facts, */
/* binds: VarMap.extend(facts.binds, assoc), */
/* }; */

/* let extend_calls = (facts, (loc, complete)) => { */
/* ...facts, */
/* calls: StmtLocMap.add(loc, complete, facts.calls), */
/* }; */
/* }; */

/* let rec analyze_calls_prog = (prog: Anf.prog, facts: Facts.t, loc: StmtLoc.t) => { */
/* let {prog_body: (body, im), _}: Anf.prog = prog; */

/* let ret = analyze_calls_last(im, facts, loc); */
/* let facts = analyze_calls_body(body, facts, StmtLoc.next(loc)); */

/* (facts, ret); */
/* } */

/* and analyze_calls_last = (last: Anf.imm, facts, loc) => { */
/* let pat_kind: Anf.pat_kind = */
/* switch (last.imm_kind) { */
/* | IConst(_) => PWild */
/* | IVar(x) => PVar(x) */
/* }; */

/* analyze_calls_pat( */
/* {pat_kind, pat_complete: NecessarilyComplete}: Anf.pat, */
/* facts, */
/* loc, */
/* ); */
/* } */

/* and analyze_calls_body = (body: list(Anf.stmt), facts, loc) => { */
/* let (facts, _) = */
/* body */
/* |> List.rev */
/* |> List.fold_left( */
/* ((facts, loc), stmt) => { */
/* let facts = analyze_calls_stmt(stmt, facts, loc); */
/* (facts, StmtLoc.next(loc)); */
/* }, */
/* (facts, loc), */
/* ); */
/* facts; */
/* } */

/* and analyze_calls_stmt = (stmt: Anf.stmt, facts, loc) => { */
/* switch (stmt.stmt_kind) { */
/* | SLet(p, c) => analyze_calls_let(p, c, facts, loc) */

/* | SLetRec(_, _) => failwith("not implemented") */
/* }; */
/* } */

/* and analyze_calls_let = (p: Anf.pat, c: Anf.comp, facts, loc) => { */
/* Analyze pattern. If it binds a variable we're searching for, we should
 * examine the right hand side. */
/* let bxs = analyze_calls_pat(p, facts, loc); */

/* switch (bxs) { */
/* | None => facts */
/* | Some(bxs) => */
/* switch (c.comp_kind) { */
/* [> Don't care about constants. <] */
/* | CImm({imm_kind: IConst(_), _}) => facts */

/* [> Track aliasing. <] */
/* | CImm({imm_kind: IVar(x), _}) => */
/* Facts.extend_binds( */
/* facts, */
/* (bxs, Tracked(TAliases([{loc, name: x}]))), */
/* ) */

/* [> Don't care about binary ops. <] */
/* | CBinOp(_, _, _) => facts */

/* [> Can't apply a constant; should be unreachable case. <] */
/* | CAp({imm_kind: IConst(_), _}, _) => */
/* failwith("bad ap with Anf.const fun") */

/* Everything starts with an application (we don't care about functions
 * that aren't applied.)
 *
 * TODO: Make all ignored functions complete? */
/* | CAp({imm_kind: IVar(fn), _}, arg) => */
/* [> Record call. <] */
/* let facts = Facts.extend_calls(facts, (loc, (arg.imm_complete, fn))); */

/* [> Start searching for the function if not already. <] */
/* let facts = */
/* switch (Facts.lookup_binds(facts, fn)) { */
/* | None => Facts.extend_binds(facts, (fn, Searching)) */
/* [> If already searching, do nothing. <] */
/* | Some(Searching) => facts */

/* This case should be unreachable: applying a variable when it was
 * tracked below. */
/* | Some(Tracked(_)) => */
/* failwith("applied function variable before declared") */
/* }; */

/* [> We now know that {bx} refers to the return of this call. <] */
/* let facts = */
/* Facts.extend_binds(facts, (bxs, Tracked(TFunReturn(fn)))); */

/* facts; */

/* If here, this function was applied somewhere (and we care about the
 * return). */
/* | CFun(param, body) => */
/* [> Analyze body. <] */
/* let (facts, ret_name) = */
/* analyze_calls_prog(body, facts, StmtLoc.nest(loc)); */

/* If the pattern defines a variable we're looking for, we need to track
 * it to param. */
/* let param_bx = analyze_calls_pat(param, facts, loc); */
/* let facts = */
/* switch (param_bx) { */
/* | Some(param_bx) => */
/* Facts.extend_binds(facts, (param_bx, Tracked(TFunParam))) */
/* | None => facts */
/* }; */

/* [> Mark as tracked to this function definition. <] */
/* let facts = */
/* Facts.extend_binds( */
/* facts, */
/* (bxs, Tracked(TFunDef({loc, ret_name}))), */
/* ); */

/* facts; */

/* [> Don't care about these. <] */
/* | CCons(_, _) */
/* | CPair(_, _) */
/* | CInj(_, _) => facts */

/* | CCase(_scrut, _rules) => failwith("not implemented") */
/* [> let scrut_loc = loc; <] */

/* [> let (facts, _) = <] */
/* [> rules <] */
/* [> |> List.rev <] */
/* [> |> List.fold_left( <] */
/* [> ((facts, loc), {rule_pat, rule_branch, _}: Anf.rule) => { <] */
/* [> [> Analyze branch. <] <] */
/* [> let (facts, ret_name) = <] */
/* [> analyze_calls_prog(rule_branch, facts, StmtLoc.nest(loc)); <] */

/* [> [> If rule pattern defines variable being searched for, track alias to scrutinee. <] <] */
/* [> let bx' = analyze_calls_pat(rule_pat, facts, loc); <] */
/* [> let facts = <] */
/* [> switch (bx') { <] */
/* [> | None => facts <] */
/* [> | Some(bx) => <] */
/* [> switch (scrut.imm_kind) { <] */
/* [> [> TODO: Not sure if this should fail? <] <] */
/* [> | IConst(_) => failwith("searching for const") <] */
/* [> | IVar(scrut_x) => <] */
/* [> let scrut_alias: Facts.alias = { <] */
/* [> loc: scrut_loc, <] */
/* [> name: scrut_x, <] */
/* [> }; <] */
/* [> Facts.extend_binds( <] */
/* [> facts, <] */
/* [> (bx, Tracked(TAliases([scrut_alias]))), <] */
/* [> ); <] */
/* [> } <] */
/* [> }; <] */

/* [> (facts, loc); <] */
/* [> }, <] */
/* [> (facts, StmtLoc.nest(loc)), <] */
/* [> ); <] */

/* [> let facts = Facts.extend_binds(facts, (bx, Tracked())) <] */
/* [> facts; <] */

/* | CEmptyHole(_u, _i, _sigma) => facts */
/* | CNonEmptyHole(_reason, _u, _i, _sigma, _im) => facts */
/* | CCast(_im, _ty, _ty') => facts */
/* } */
/* }; */
/* } */

/* Analyze a pattern and return the list of bound variables (that are being
 * searched for). */
/* and analyze_calls_pat = (p: Anf.pat, facts, loc) => { */
/* switch (p.pat_kind) { */
/* | PWild => None */

/* | PVar(x) => */
/* switch (Facts.lookup_binds(facts, x)) { */
/* [> If not found, this binding doesn't matter. <] */
/* | None => None */
/* [> If searching, return it to be bound.  <] */
/* | Some(Searching) => Some([x]) */

/* [> These case should be impossible? <] */
/* | Some(FoundAliases(_aliases)) => failwith("not single assignment") */
/* | Some(FoundFun(_f)) => failwith("not single assignment") */
/* } */

/* | PInt(_) */
/* | PFloat(_) */
/* | PBool(_) */
/* | PNil => None */

/* | PInj(_side, p') => analyze_calls_pat(p', facts, loc) */
/* | PCons(p1, p2) */
/* | PPair(p1, p2) => */
/* let bxs = analyze_calls_pat(p1, facts, loc); */
/* let bxs' = analyze_calls_pat(p2, facts, loc); */
/* switch (bxs, bxs') { */
/* | (Some(bxs), Some(bxs')) => Some(bxs @ bxs') */
/* | (Some(bxs), None) */
/* | (None, Some(bxs)) => Some(bxs) */
/* | (None, None) => None */
/* }; */

/* | PTriv => None */
/* }; */
/* }; */

/* let rec analyze_fix_prog = (prog: Anf.prog, facts: Facts.t, loc: StmtLoc.t) => { */
/* let {prog_body: (body, im), prog_ty, prog_complete: _}: Anf.prog = prog; */

/* let body = analyze_fix_body(body, facts, loc); */

/* prog; */
/* } */

/* and analyze_fix_body = (body: list(Anf.stmt), facts, loc) => { */
/* body */
/* |> List.fold_left( */
/* ((expanded_stmts, loc), stmt) => { */
/* let new_stmts = analyze_fix_stmt(stmt, facts, loc); */
/* ([new_stmts, ...expanded_stmts], StmtLoc.next(loc)); */
/* }, */
/* ([], loc), */
/* ) */
/* |> (((expanded_stmts, _)) => expanded_stmts) */
/* |> List.rev */
/* |> List.flatten; */
/* } */

/* and analyze_fix_stmt = */
/* ({stmt_kind, stmt_complete: _}: Anf.stmt, _facts, _loc) => { */
/* switch (stmt_kind) { */
/* | SLet(_p, _c) => failwith("not implemented") */

/* | SLetRec(_, _) => failwith("not implemented") */
/* }; */
/* }; */

let analyze = (prog: Mir_anf.prog): Mir_anf.prog => {
  /* let facts = analyze_calls_prog(prog, Facts.init, StmtLoc.init); */
  /* let prog = analyze_fix_prog(prog, facts, StmtLoc.init); */
  prog;
};
