open DrvTermBase;

/**
  This module describles the speculation of rules for checking
  involving calculations. Refer to `RuleSpec.re` For speculations
  on unboxing and unification,
 */

type ctx_t = list(exp_t);

type t('a) =
  | LookUpExp(Var.t): t(exp_t)
  | LookUpPat(Var.t): t(pat_t)
  | LookUpTyp(Var.t): t(typ_t)
  | LookUpTPat(Var.t): t(tpat_t)
  | UnboxCtx(t(exp_t)): t(ctx_t)
  | UnboxNumLit(t(exp_t)): t(int)
  | UnboxExpVar(t(exp_t)): t(Var.t)
  | UnboxPatVar(t(pat_t)): t(Var.t)
  | UnboxTypVar(t(typ_t)): t(Var.t)
  | UnboxTPatVar(t(tpat_t)): t(Var.t)
  | ExpVar(t(Var.t)): t(exp_t)
  | HasType(t(exp_t), t(typ_t)): t(exp_t)
  | Type(t(typ_t)): t(exp_t)
  | Fix(t(pat_t), t(exp_t)): t(exp_t)
  | Subst(t(exp_t), t(Var.t), t(exp_t)): t(exp_t)
  | Ctx(t(ctx_t)): t(exp_t)
  | Cons(t(exp_t), t(ctx_t)): t(ctx_t)
  | Neg(t(int)): t(int)
  | Plus(t(int), t(int)): t(int)
  | Minus(t(int), t(int)): t(int)
  | Times(t(int), t(int)): t(int)
  | TypVar(t(Var.t)): t(typ_t)
  | Rec(t(tpat_t), t(typ_t)): t(typ_t)
  | Glb(t(typ_t), t(typ_t)): t(typ_t)
  | SubstTy(t(typ_t), t(Var.t), t(typ_t)): t(typ_t)
  | Ignore(t('a)): t(bool)
  | Gt(t(int), t(int)): t(bool)
  | Lt(t(int), t(int)): t(bool)
  | Eq(t(int), t(int)): t(bool)
  | NotGt(t(int), t(int)): t(bool)
  | NotLt(t(int), t(int)): t(bool)
  | NotEq(t(int), t(int)): t(bool)
  | EqExp(t(exp_t), t(exp_t)): t(bool)
  | EqCtx(t(ctx_t), t(ctx_t)): t(bool)
  | EqTyp(t(typ_t), t(typ_t)): t(bool)
  | Mem(t(exp_t), t(ctx_t)): t(bool)
  | Subset(t(ctx_t), t(ctx_t)): t(bool);

[@deriving (show({with_path: false}), sexp, yojson)]
type test = [@opaque] t(bool);

[@deriving (show({with_path: false}), sexp, yojson)]
type map = RuleSpec.map;

[@deriving (show({with_path: false}), sexp, yojson)]
type failure =
  | FailUnbox(RuleSpec.specced, Drv.Any.cls)
  | FailTest(map, test);

//   let rec show_linked = (p, map: RuleSpec.map, op) =>
//     switch (op) {
//     | Get(s) =>
//       switch (RuleSpec.Map.find_opt(s, map)) {
//       | Some(specced) => RuleSpec.show_linked(specced)
//       | None => s
//       }
//     | _ =>
//       op
//       |> repr(p)
//       |> Aba.join(Fun.id, show_linked(precedence(op), map))
//       |> String.concat("")
//     };

// let show_linked = (map: RuleSpec.map, test: t) =>
//   test
//   |> repr
//   |> (
//     ((labels, ops) as aba) =>
//       switch (test, ops) {
//       | (Eq(Get(_), _), [a, b]) => (labels, [a, b])
//       | _ => aba
//       }
//   )
//   |> Aba.join(Fun.id, Operation.show_linked(map))
//   |> String.concat("");

let failure_msg = (failure: failure): string =>
  switch (failure) {
  | FailUnbox(specced, cls) =>
    Printf.sprintf(
      "Failed to match %s with %s",
      RuleSpec.show_linked(specced),
      cls |> Drv.Any.show_cls,
    )
  // TODO(zhiyao): show the test
  | FailTest(_, _) => Printf.sprintf("Failed to verify a test")
  };

let rec get_symbols: type a. t(a) => list(string) =
  fun
  | LookUpExp(s) => [s]
  | LookUpPat(s) => [s]
  | LookUpTyp(s) => [s]
  | LookUpTPat(s) => [s]
  | UnboxCtx(a) => get_symbols(a)
  | UnboxNumLit(a) => get_symbols(a)
  | UnboxExpVar(a) => get_symbols(a)
  | UnboxPatVar(a) => get_symbols(a)
  | UnboxTypVar(a) => get_symbols(a)
  | UnboxTPatVar(a) => get_symbols(a)
  | ExpVar(a) => get_symbols(a)
  | HasType(a, b) => get_symbols(a) @ get_symbols(b)
  | Type(a) => get_symbols(a)
  | Fix(a, b) => get_symbols(a) @ get_symbols(b)
  | Subst(a, b, c) => get_symbols(a) @ get_symbols(b) @ get_symbols(c)
  | Ctx(a) => get_symbols(a)
  | Cons(a, b) => get_symbols(a) @ get_symbols(b)
  | Neg(a) => get_symbols(a)
  | Plus(a, b) => get_symbols(a) @ get_symbols(b)
  | Minus(a, b) => get_symbols(a) @ get_symbols(b)
  | Times(a, b) => get_symbols(a) @ get_symbols(b)
  | TypVar(a) => get_symbols(a)
  | Rec(a, b) => get_symbols(a) @ get_symbols(b)
  | Glb(a, b) => get_symbols(a) @ get_symbols(b)
  | SubstTy(a, b, c) => get_symbols(a) @ get_symbols(b) @ get_symbols(c)
  | Ignore(a) => get_symbols(a)
  | Gt(a, b) => get_symbols(a) @ get_symbols(b)
  | Lt(a, b) => get_symbols(a) @ get_symbols(b)
  | Eq(a, b) => get_symbols(a) @ get_symbols(b)
  | NotGt(a, b) => get_symbols(a) @ get_symbols(b)
  | NotLt(a, b) => get_symbols(a) @ get_symbols(b)
  | NotEq(a, b) => get_symbols(a) @ get_symbols(b)
  | EqExp(a, b) => get_symbols(a) @ get_symbols(b)
  | EqCtx(a, b) => get_symbols(a) @ get_symbols(b)
  | EqTyp(a, b) => get_symbols(a) @ get_symbols(b)
  | Mem(a, b) => get_symbols(a) @ get_symbols(b)
  | Subset(a, b) => get_symbols(a) @ get_symbols(b);

exception Unreachable;
exception Failure(failure);

let go = (map: map, test: test): option(failure) => {
  let unquote = s =>
    switch (RuleSpec.Map.find_opt(s, map)) {
    | Some(specced) => specced
    | None => raise(Unreachable)
    };
  let exp_of_specced =
    fun
    | (_, Exp(syntax)) => syntax
    | _ => raise(Unreachable);
  let pat_of_specced =
    fun
    | (_, Pat(syntax)) => syntax
    | _ => raise(Unreachable);
  let typ_of_specced =
    fun
    | (_, Typ(syntax)) => syntax
    | _ => raise(Unreachable);
  let tpat_of_specced =
    fun
    | (_, TPat(syntax)) => syntax
    | _ => raise(Unreachable);
  let rec go: type a. t(a) => a =
    formula =>
      switch (formula) {
      | LookUpExp(s) => s |> unquote |> exp_of_specced
      | LookUpPat(s) => s |> unquote |> pat_of_specced
      | LookUpTyp(s) => s |> unquote |> typ_of_specced
      | LookUpTPat(s) => s |> unquote |> tpat_of_specced
      | UnboxCtx(LookUpExp(s)) =>
        let specced = unquote(s);
        switch (Drv.Exp.term_of(exp_of_specced(specced))) {
        | Ctx(syntax) => syntax
        | _ => raise(Failure(FailUnbox(specced, Exp(Ctx))))
        };
      | UnboxCtx(_) => raise(Unreachable)
      | UnboxNumLit(LookUpExp(s)) =>
        let specced = unquote(s);
        switch (Drv.Exp.term_of(exp_of_specced(specced))) {
        | NumLit(i) => i
        | Neg({term: NumLit(i), _}) => - i
        | _ => raise(Failure(FailUnbox(specced, Exp(NumLit))))
        };
      | UnboxNumLit(_) => raise(Unreachable)
      | UnboxExpVar(LookUpExp(s)) =>
        let specced = unquote(s);
        switch (Drv.Exp.term_of(exp_of_specced(specced))) {
        | Var(s) => s
        | _ => raise(Failure(FailUnbox(specced, Exp(Var))))
        };
      | UnboxExpVar(_) => raise(Unreachable)
      | UnboxPatVar(LookUpPat(s)) =>
        let specced = unquote(s);
        let rec f = p =>
          switch (Drv.Pat.term_of(p)) {
          | Var(s) => s
          | Cast(p, _) => f(p)
          | _ => raise(Failure(FailUnbox(specced, Pat(Var))))
          };
        f(pat_of_specced(specced));
      | UnboxTypVar(LookUpTyp(s)) =>
        let specced = unquote(s);
        switch (Drv.Typ.term_of(typ_of_specced(specced))) {
        | Var(s) => s
        | _ => raise(Failure(FailUnbox(specced, Typ(Var))))
        };
      | UnboxTypVar(_) => raise(Unreachable)
      | UnboxTPatVar(LookUpTPat(s)) =>
        let specced = unquote(s);
        switch (Drv.TPat.term_of(tpat_of_specced(specced))) {
        | Var(s) => s
        | _ => raise(Failure(FailUnbox(specced, TPat(Var))))
        };
      | ExpVar(s) => Var(go(s)) |> Drv.Exp.fresh
      | HasType(e, t) => HasType(go(e), go(t)) |> Drv.Exp.fresh
      | Type(t) => Type(go(t)) |> Drv.Exp.fresh
      | Fix(p, e) => Fix(go(p), go(e)) |> Drv.Exp.fresh
      | Subst(e, p, e') => Drv.Exp.subst(go(e), go(p), go(e'))
      | Ctx(e) => Ctx(go(e)) |> Drv.Exp.fresh
      | Cons(e, ctx) => Drv.Exp.cons_ctx(go(ctx), go(e))
      | Neg(e) => - go(e)
      | Plus(e1, e2) => go(e1) + go(e2)
      | Minus(e1, e2) => go(e1) - go(e2)
      | Times(e1, e2) => go(e1) * go(e2)
      | TypVar(s) => Var(go(s)) |> Drv.Typ.fresh
      | Rec(t, a) => Rec(go(t), go(a)) |> Drv.Typ.fresh
      | Glb(a, b) => Drv.Typ.glb(go(a), go(b))
      | SubstTy(v, x, e) => Drv.Typ.subst(go(v), go(x), go(e))
      | Ignore(a) => go(a) |> (_ => true)
      | Gt(a, b) => go(a) > go(b)
      | Lt(a, b) => go(a) < go(b)
      | Eq(a, b) => go(a) == go(b)
      | NotGt(a, b) => go(a) <= go(b)
      | NotLt(a, b) => go(a) >= go(b)
      | NotEq(a, b) => go(a) != go(b)
      | EqExp(a, b) => Drv.Exp.eq(go(a), go(b))
      | EqCtx(a, b) => List.equal(Drv.Exp.eq, go(a), go(b))
      | EqTyp(a, b) => Drv.Typ.eq(go(a), go(b))
      | Mem(a, b) => Drv.Exp.mem_ctx(go(a), go(b))
      | Subset(a, b) => Drv.Exp.subset_ctx(go(a), go(b))
      };
  try(go(test) ? None : Some(FailTest(map, test))) {
  | Failure(failure) => Some(failure)
  | Unreachable => failwith("Unreachable")
  | _ => None
  };
};

// module Operation = {
//   [@deriving (show({with_path: false}), sexp, yojson)]
//   type t =
//     // Get what has been registered
//     | Get(string)
//     // These t do not take effect on frontend, but is essential
//     | VarOfPat(t) // VarOfPat(Pat(_))
//     | TVarOfTPat(t) // TVarOfTPat(TPat(_))
//     // These t perform syntax reconstruction
//     | HasType(t, t) // HasType(Var(_), _)
//     | Type(t) // Type(TVar(_))
//     | Fix(t, t) // Fix(Pat(_), _)
//     | Rec(t, t) // Rec(TPat(_), _)
//     // These t perform pure calculation
//     | Glb(t, t) // Glb(_, _)
//     | Subst((t, t), t) // Subst((_, Pat(_)), _)
//     | SubstTy((t, t), t) // SubstTy((_, TPat(_)), _)
//     | Cons(t, t) // Cons(_, Ctx(_))
//     | Neg(t) // Neg(NumLit(_))
//     | Plus(t, t) // Plus(NumLit(_), NumLit(_))
//     | Minus(t, t) // Minus(NumLit(_), NumLit(_))
//     | Times(t, t); // Times(NumLit(_), NumLit(_))

//   let precedence: t => int = {
//     module P = Precedence;
//     fun
//     | Get(_) => P.max
//     | VarOfPat(_) => P.max
//     | TVarOfTPat(_) => P.max
//     | HasType(_, _) => P.cast
//     | Type(_) => P.cast
//     | Fix(_) => P.fun_
//     | Rec(_) => P.type_arrow + 2
//     | Glb(_, _) => P.max
//     | Subst(_) => P.ap
//     | SubstTy(_) => P.ap
//     | Cons(_) => P.comma
//     | Neg(_) => P.neg
//     | Plus(_) => P.plus
//     | Minus(_) => P.plus
//     | Times(_) => P.mult;
//   };

//   let repr = (~sp: string=" ", p: int, operation: t): Aba.t(string, t) => {
//     let p' = precedence(operation);
//     let tight_start = s =>
//       s == ""
//       || List.exists(
//            String.ends_with(s, ~suffix=_),
//            ["/", "「", "」", "("],
//          );
//     let tight_end = s =>
//       s == ""
//       || List.exists(
//            String.starts_with(s, ~prefix=_),
//            ["/", "」", ",", ")"],
//          );
//     let mk_parens = labels =>
//       labels
//       |> ListUtil.map_first(s => p < p' ? "(" ++ s : s)
//       |> ListUtil.map_last(s => p < p' ? s ++ ")" : s);
//     let op = labels =>
//       labels
//       |> List.map(s =>
//            (tight_end(s) ? "" : sp) ++ s ++ (tight_start(s) ? "" : sp)
//          )
//       |> ListUtil.map_first(s =>
//            String.trim(s) ++ (tight_start(s) ? "" : sp)
//          )
//       |> ListUtil.map_last(s => (tight_end(s) ? "" : sp) ++ String.trim(s))
//       |> mk_parens;
//     let bin = (labels: list(string)) => op([""] @ labels @ [""]);
//     let pre = (labels: list(string)) => op(labels @ [""]);
//     let post = (labels: list(string)) => op([""] @ labels);
//     let op_sg = (label: string) => [label];
//     let bin_sg = (label: string) => bin([label]);
//     let pre_sg = (label: string) => pre([label]);
//     let post_sg = (label: string) => post([label]);
//     switch (operation) {
//     | Get(s) => (s |> op_sg, [])
//     | VarOfPat(p) => ([] |> bin, [p])
//     | TVarOfTPat(t) => ([] |> bin, [t])
//     | HasType(x, t) => (":" |> bin_sg, [x, t])
//     | Type(a) => ("type" |> post_sg, [a])
//     | Fix(p, e) => (["fix", "→"] |> pre, [p, e])
//     | Rec(t, a) => (["rec", "is"] |> pre, [t, a])
//     | Glb(a, b) => (["glb(", ",", ")"] |> op, [a, b])
//     | Subst((v, x), e) => (["「", "/", "」"] |> pre, [v, x, e])
//     | SubstTy((t, a), e) => (["「", "/", "」"] |> pre, [t, a, e])
//     | Cons(e, l) => ("," |> bin_sg, [l, e])
//     | Neg(n) => ("-" |> pre_sg, [n])
//     | Plus(n1, n2) => ("+" |> bin_sg, [n1, n2])
//     | Minus(n1, n2) => ("-" |> bin_sg, [n1, n2])
//     | Times(n1, n2) => ("×" |> bin_sg, [n1, n2])
//     };
//   };

//   let rec show = (p, syntax) =>
//     syntax
//     |> repr(p)
//     |> Aba.join(Fun.id, show(precedence(syntax)))
//     |> String.concat("");

//   let show = show(Precedence.min);

//   let show_linked = show_linked(Precedence.min);

// let repr = (~sp: string=" ", test: t): Aba.t(string, Operation.t) => {
//   let op = labels =>
//     labels
//     |> List.map(s => sp ++ s ++ sp)
//     |> ListUtil.map_first(s => String.trim(s))
//     |> ListUtil.map_last(s => String.trim(s));
//   let bin = (labels: list(string)) => op([""] @ labels @ [""]);
//   let bin_sg = (label: string) => bin([label]);
//   switch (test) {
//   | Eq(a, b) => ("=" |> bin_sg, [a, b])
//   | NotEq(a, b) => ("≠" |> bin_sg, [a, b])
//   | Lt(a, b) => ("<" |> bin_sg, [a, b])
//   | NotLt(a, b) => ("≥" |> bin_sg, [a, b])
//   | Gt(a, b) => (">" |> bin_sg, [a, b])
//   | NotGt(a, b) => ("≤" |> bin_sg, [a, b])
//   | Mem(p, ctx) => ("∈" |> bin_sg, [p, ctx])
//   | Subset(a, b) => ("⊆" |> bin_sg, [a, b])
//   };
// };

// let show = syntax =>
//   syntax |> repr |> Aba.join(Fun.id, Operation.show) |> String.concat("");

let of_tests: Rule.t => list(test) = {
  module SymbolMap =
    SymbolMap.M({
      type exp = t(exp_t);
      type pat = t(pat_t);
      type typ = t(typ_t);
      type tpat = t(tpat_t);
      let exp: string => exp = s => LookUpExp(s);
      let pat: string => pat = s => LookUpPat(s);
      let typ: string => typ = s => LookUpTyp(s);
      let tpat: string => tpat = s => LookUpTPat(s);
    });
  SymbolMap.(
    fun
    // Type consistency
    | C_Refl
    | C_UnkL
    | C_UnkR
    | C_Sum
    | C_Prod
    | C_Arrow => []
    // Type Matched
    | MS_Hole
    | MS_Sum
    | MP_Hole
    | MP_Prod
    | MA_Hole
    | MA_Arrow => []
    // Type Validity
    | TV_Num
    | TV_Bool
    | TV_Unit
    | TV_Arrow
    | TV_Prod
    | TV_Sum => []
    | TV_Rec => [
        EqCtx(
          UnboxCtx(delta'),
          Cons(Type(TypVar(UnboxTPatVar(tpat))), UnboxCtx(delta)),
        ),
      ]
    | TV_TVar => [Mem(Type(t), UnboxCtx(delta))]
    // Typing
    | A_Subsumption
    | A_Subsumption_GT => []
    | S_Hole => []
    | T_True
    | S_True => []
    | T_False
    | S_False => []
    | T_If
    | S_If
    | A_If => []
    | S_If_GT => [EqTyp(t, Glb(t1, t2))]
    | T_Num
    | S_Num => [Ignore(UnboxNumLit(n))]
    | T_Neg
    | S_Neg => []
    | T_Plus
    | S_Plus => []
    | T_Minus
    | S_Minus => []
    | T_Times
    | S_Times => []
    | T_Lt
    | S_Lt => []
    | T_Gt
    | S_Gt => []
    | T_Eq
    | S_Eq => []
    | T_Var
    | S_Var => [Mem(HasType(ExpVar(UnboxExpVar(e)), t), UnboxCtx(gamma))]
    | T_LetAnn_TV => [
        EqCtx(
          UnboxCtx(gamma'),
          Cons(HasType(ExpVar(UnboxPatVar(x)), t_def), UnboxCtx(gamma)),
        ),
        Subset(UnboxCtx(delta), UnboxCtx(gamma)),
      ]
    | T_LetAnn
    | S_LetAnn
    | A_LetAnn => [Ignore(UnboxPatVar(x))]
    | T_Let
    | S_Let
    | A_Let => [
        EqCtx(
          UnboxCtx(gamma'),
          Cons(HasType(ExpVar(UnboxPatVar(x)), t_def), UnboxCtx(gamma)),
        ),
      ]
    | T_FunAnn_TV => [
        EqCtx(
          UnboxCtx(gamma'),
          Cons(HasType(ExpVar(UnboxPatVar(x)), t_in), UnboxCtx(gamma)),
        ),
        Subset(UnboxCtx(delta), UnboxCtx(gamma)),
      ]
    | T_FunAnn
    | S_FunAnn
    | A_FunAnn => [Ignore(UnboxPatVar(x))]
    | T_Fun
    | A_Fun
    | A_Fun_GT => [
        EqCtx(
          UnboxCtx(gamma'),
          Cons(HasType(ExpVar(UnboxPatVar(x)), t_in), UnboxCtx(gamma)),
        ),
      ]
    | A_FunAnn_GT => [
        EqCtx(
          UnboxCtx(gamma'),
          Cons(HasType(ExpVar(UnboxPatVar(x)), t_in'), UnboxCtx(gamma)),
        ),
      ]
    | T_Ap
    | S_Ap
    | S_Ap_GT => []
    | T_Triv
    | S_Triv => []
    | T_Pair
    | S_Pair
    | A_Pair
    | A_Pair_GT => []
    | T_LetPair
    | S_LetPair
    | S_LetPair_GT
    | A_LetPair
    | A_LetPair_GT => [
        EqCtx(
          UnboxCtx(gamma'),
          Cons(
            HasType(ExpVar(UnboxPatVar(y)), t2),
            Cons(HasType(ExpVar(UnboxPatVar(x)), t1), UnboxCtx(gamma)),
          ),
        ),
      ]
    | T_PrjL
    | S_PrjL
    | S_PrjL_GT
    | T_PrjR
    | S_PrjR
    | S_PrjR_GT => []
    | T_InjL
    | A_InjL
    | A_InjL_GT
    | T_InjR
    | A_InjR
    | A_InjR_GT => []
    | T_Case
    | S_Case
    | A_Case
    | A_Case_GT => [
        EqCtx(
          UnboxCtx(gamma'),
          Cons(HasType(ExpVar(UnboxPatVar(x)), t1), UnboxCtx(gamma)),
        ),
        EqCtx(
          UnboxCtx(gamma''),
          Cons(HasType(ExpVar(UnboxPatVar(y)), t2), UnboxCtx(gamma)),
        ),
      ]
    | S_Case_GT => [
        EqCtx(
          UnboxCtx(gamma'),
          Cons(HasType(ExpVar(UnboxPatVar(x)), t1), UnboxCtx(gamma)),
        ),
        EqCtx(
          UnboxCtx(gamma''),
          Cons(HasType(ExpVar(UnboxPatVar(y)), t2), UnboxCtx(gamma)),
        ),
        EqTyp(t', Glb(t1', t2')),
      ]
    | T_FixAnn_TV => [
        EqCtx(
          UnboxCtx(gamma'),
          Cons(HasType(ExpVar(UnboxPatVar(x)), t), UnboxCtx(gamma)),
        ),
        Subset(UnboxCtx(delta), UnboxCtx(gamma)),
      ]
    | T_FixAnn
    | T_Fix => [
        EqCtx(
          UnboxCtx(gamma'),
          Cons(HasType(ExpVar(UnboxPatVar(x)), t), UnboxCtx(gamma)),
        ),
      ]
    | T_Roll
    | T_Unroll => [
        EqTyp(
          t_body',
          SubstTy(Rec(tpat, t_body), UnboxTPatVar(tpat), t_body),
        ),
      ]
    // Evaluation
    | E_If_T => []
    | E_If_F => []
    | E_Num => [Ignore(UnboxNumLit(n))]
    | E_Neg => [Eq(UnboxNumLit(n'), Neg(UnboxNumLit(n)))]
    | E_Plus => [
        Eq(UnboxNumLit(n'), Plus(UnboxNumLit(n1), UnboxNumLit(n2))),
      ]
    | E_Minus => [
        Eq(UnboxNumLit(n'), Minus(UnboxNumLit(n1), UnboxNumLit(n2))),
      ]
    | E_Times => [
        Eq(UnboxNumLit(n'), Times(UnboxNumLit(n1), UnboxNumLit(n2))),
      ]
    | E_Lt_T => [Lt(UnboxNumLit(n1), UnboxNumLit(n2))]
    | E_Lt_F => [NotLt(UnboxNumLit(n1), UnboxNumLit(n2))]
    | E_Gt_T => [Gt(UnboxNumLit(n1), UnboxNumLit(n2))]
    | E_Gt_F => [NotGt(UnboxNumLit(n1), UnboxNumLit(n2))]
    | E_Eq_T => [Eq(UnboxNumLit(n1), UnboxNumLit(n2))]
    | E_Eq_F => [NotEq(UnboxNumLit(n1), UnboxNumLit(n2))]
    | E_Let => [EqExp(e_body', Subst(v_def, UnboxPatVar(x), e_body))]
    | E_Ap => [EqExp(e_body', Subst(v2, UnboxPatVar(x), e_body))]
    | E_Pair => []
    | E_LetPair => [
        EqExp(
          e_body',
          Subst(v1, UnboxPatVar(x), Subst(v2, UnboxPatVar(y), e_body)),
        ),
      ]
    | E_PrjL => []
    | E_PrjR => []
    | E_InjL => []
    | E_InjR => []
    | E_Case_L => [EqExp(e1', Subst(v, UnboxPatVar(x), e1))]
    | E_Case_R => [EqExp(e2', Subst(v, UnboxPatVar(y), e2))]
    | E_Fix => [EqExp(e', Subst(Fix(x, e_body), UnboxPatVar(x), e_body))]
    | E_Roll => []
    | E_Unroll => []
    | E_Val => []
    // Values
    | V_True
    | V_False => []
    | V_Num => [Ignore(UnboxNumLit(n))]
    | V_Fun => [Ignore(UnboxPatVar(x))]
    | V_Triv
    | V_Pair
    | V_InjL
    | V_InjR
    | V_Roll => []
    // Logical Proposition
    | Assumption => [Mem(a, UnboxCtx(gamma))]
    | And_I => []
    | And_E_L => []
    | And_E_R => []
    | Or_I_L => []
    | Or_I_R => []
    | Or_E => [
        EqCtx(UnboxCtx(gamma'), Cons(a, UnboxCtx(gamma))),
        EqCtx(UnboxCtx(gamma''), Cons(b, UnboxCtx(gamma))),
      ]
    | Implies_I => [EqCtx(UnboxCtx(gamma'), Cons(a, UnboxCtx(gamma)))]
    | Implies_E => []
    | Truth_I => []
    | Falsity_E => []
  );
};
