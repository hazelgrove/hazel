open Util;

let rec string_of_pat = syntax =>
  switch (DrvSyntax.term_of(syntax)) {
  | Pat(s) => s
  | Cast(p, _) => string_of_pat(p)
  | _ => failwith("string_of_pat: expected Pat")
  };
let string_of_tpat = syntax =>
  switch (DrvSyntax.term_of(syntax)) {
  | TPat(s) => s
  | _ => failwith("string_of_pat: expected Pat")
  };
let rec int_of_numlit = syntax =>
  switch (DrvSyntax.term_of(syntax)) {
  | NumLit(i) => i
  | Neg(syntax') => - int_of_numlit(syntax')
  | _ => failwith("int_of_numlit: expected NumLit")
  };
let list_of_ctx = syntax =>
  switch (DrvSyntax.term_of(syntax)) {
  | Ctx(xs) => xs
  | _ => failwith("list_of_ctx: expected Ctx")
  };

module Operation = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    // Get what has been registered
    | Get(string)
    // These t do not take effect on frontend, but is essential
    | VarOfPat(t) // VarOfPat(Pat(_))
    | TVarOfTPat(t) // TVarOfTPat(TPat(_))
    // These t perform syntax reconstruction
    | HasType(t, t) // HasType(Var(_), _)
    | Type(t) // Type(TVar(_))
    | Fix(t, t) // Fix(Pat(_), _)
    | Rec(t, t) // Rec(TPat(_), _)
    // These t perform pure calculation
    | Subst((t, t), t) // Subst((_, Pat(_)), _)
    | SubstTy((t, t), t) // SubstTy((_, TPat(_)), _)
    | Cons(t, t) // Cons(_, Ctx(_))
    | Neg(t) // Neg(NumLit(_))
    | Plus(t, t) // Plus(NumLit(_), NumLit(_))
    | Minus(t, t) // Minus(NumLit(_), NumLit(_))
    | Times(t, t); // Times(NumLit(_), NumLit(_))

  let precedence: t => int = {
    module P = Precedence;
    fun
    | Get(_) => P.max
    | VarOfPat(_) => P.max
    | TVarOfTPat(_) => P.max
    | HasType(_, _) => P.ann
    | Type(_) => P.ann
    | Fix(_) => P.fun_
    | Rec(_) => P.type_arrow + 2
    | Subst(_) => P.ap
    | SubstTy(_) => P.ap
    | Cons(_) => P.comma
    | Neg(_) => P.neg
    | Plus(_) => P.plus
    | Minus(_) => P.plus
    | Times(_) => P.mult;
  };

  let repr = (~sp: string=" ", p: int, operation: t): Aba.t(string, t) => {
    let p' = precedence(operation);
    let tight_start = s =>
      s == ""
      || List.exists(String.ends_with(s, ~suffix=_), ["/", "「", "」"]);
    let tight_end = s =>
      s == ""
      || List.exists(String.starts_with(s, ~prefix=_), ["/", "」", ","]);
    let mk_parens = labels =>
      labels
      |> ListUtil.map_first(s => p < p' ? "(" ++ s : s)
      |> ListUtil.map_last(s => p < p' ? s ++ ")" : s);
    let op = labels =>
      labels
      |> List.map(s =>
           (tight_end(s) ? "" : sp) ++ s ++ (tight_start(s) ? "" : sp)
         )
      |> ListUtil.map_first(s =>
           String.trim(s) ++ (tight_start(s) ? "" : sp)
         )
      |> ListUtil.map_last(s => (tight_end(s) ? "" : sp) ++ String.trim(s))
      |> mk_parens;
    let bin = (labels: list(string)) => op([""] @ labels @ [""]);
    let pre = (labels: list(string)) => op(labels @ [""]);
    let post = (labels: list(string)) => op([""] @ labels);
    let op_sg = (label: string) => [label];
    let bin_sg = (label: string) => bin([label]);
    let pre_sg = (label: string) => pre([label]);
    let post_sg = (label: string) => post([label]);
    switch (operation) {
    | Get(s) => (s |> op_sg, [])
    | VarOfPat(p) => ([] |> bin, [p])
    | TVarOfTPat(t) => ([] |> bin, [t])
    | HasType(x, t) => (":" |> bin_sg, [x, t])
    | Type(a) => ("type" |> post_sg, [a])
    | Fix(p, e) => (["fix", "→"] |> pre, [p, e])
    | Rec(t, a) => (["rec", "is"] |> pre, [t, a])
    | Subst((v, x), e) => (["「", "/", "」"] |> pre, [v, x, e])
    | SubstTy((t, a), e) => (["「", "/", "」"] |> pre, [t, a, e])
    | Cons(e, l) => ("," |> bin_sg, [l, e])
    | Neg(n) => ("-" |> pre_sg, [n])
    | Plus(n1, n2) => ("+" |> bin_sg, [n1, n2])
    | Minus(n1, n2) => ("-" |> bin_sg, [n1, n2])
    | Times(n1, n2) => ("×" |> bin_sg, [n1, n2])
    };
  };

  let rec show = (p, syntax) =>
    syntax
    |> repr(p)
    |> Aba.join(Fun.id, show(precedence(syntax)))
    |> String.concat("");

  let show = show(Precedence.min);

  let rec show_linked = (p, map: RuleSpec.map, op) =>
    switch (op) {
    | Get(s) =>
      switch (RuleSpec.Map.find_opt(s, map)) {
      | Some(specced) => RuleSpec.show_linked(specced)
      | None => s
      }
    | _ =>
      op
      |> repr(p)
      |> Aba.join(Fun.id, show_linked(precedence(op), map))
      |> String.concat("")
    };

  let show_linked = show_linked(Precedence.min);

  let rec get_symbols: t => list(string) =
    fun
    | Get(s) => [s]
    | VarOfPat(p) => get_symbols(p)
    | TVarOfTPat(t) => get_symbols(t)
    | HasType(x, t) => get_symbols(x) @ get_symbols(t)
    | Type(a) => get_symbols(a)
    | Fix(p, e) => get_symbols(p) @ get_symbols(e)
    | Rec(t, a) => get_symbols(t) @ get_symbols(a)
    | Subst((v, x), e) => get_symbols(v) @ get_symbols(x) @ get_symbols(e)
    | SubstTy((t, a), e) =>
      get_symbols(t) @ get_symbols(a) @ get_symbols(e)
    | Cons(e, l) => get_symbols(e) @ get_symbols(l)
    | Neg(n) => get_symbols(n)
    | Plus(n1, n2) => get_symbols(n1) @ get_symbols(n2)
    | Minus(n1, n2) => get_symbols(n1) @ get_symbols(n2)
    | Times(n1, n2) => get_symbols(n1) @ get_symbols(n2);

  let rec go: (RuleSpec.map, t) => DrvSyntax.t =
    (map, op) => {
      let go = go(map);
      switch (op) {
      | Get(s) =>
        switch (RuleSpec.Map.find_opt(s, map)) {
        | Some((_, syntax)) => syntax
        | None => Hole("Not found: " ++ s) |> DrvSyntax.fresh
        }
      | VarOfPat(p) =>
        let p = go(p);
        let (_, rewrap: DrvSyntax.term => 'b) = IdTagged.unwrap(p);
        Var(p |> string_of_pat) |> rewrap;
      | TVarOfTPat(t) =>
        let t = go(t);
        let (_, rewrap: DrvSyntax.term => 'b) = IdTagged.unwrap(t);
        TVar(t |> string_of_tpat) |> rewrap;
      | HasType(x, t) => HasType(go(x), go(t)) |> DrvSyntax.fresh
      | Type(a) => Type(go(a)) |> DrvSyntax.fresh
      | Fix(p, e) => Fix(go(p), go(e)) |> DrvSyntax.fresh
      | Rec(t, a) => Rec(go(t), go(a)) |> DrvSyntax.fresh
      | Subst((v, x), e) =>
        DrvSyntax.subst(go(v), go(x) |> string_of_pat, go(e))
      | SubstTy((t, a), e) =>
        DrvSyntax.subst_ty(go(t), go(a) |> string_of_tpat, go(e))
      | Cons(p, ctx) =>
        let p = go(p);
        let ctx = go(ctx) |> list_of_ctx;
        Ctx(DrvSyntax.cons_ctx(ctx, p)) |> DrvSyntax.fresh;
      | Neg(n) =>
        let n = go(n) |> int_of_numlit;
        NumLit(- n) |> DrvSyntax.fresh;
      | Plus(n1, n2) =>
        let n1 = go(n1) |> int_of_numlit;
        let n2 = go(n2) |> int_of_numlit;
        NumLit(n1 + n2) |> DrvSyntax.fresh;
      | Minus(n1, n2) =>
        let n1 = go(n1) |> int_of_numlit;
        let n2 = go(n2) |> int_of_numlit;
        NumLit(n1 - n2) |> DrvSyntax.fresh;
      | Times(n1, n2) =>
        let n1 = go(n1) |> int_of_numlit;
        let n2 = go(n2) |> int_of_numlit;
        NumLit(n1 * n2) |> DrvSyntax.fresh;
      };
    };
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Eq(Operation.t, Operation.t)
  | NotEq(Operation.t, Operation.t)
  | Lt(Operation.t, Operation.t)
  | NotLt(Operation.t, Operation.t)
  | Gt(Operation.t, Operation.t)
  | NotGt(Operation.t, Operation.t)
  | Mem(Operation.t, Operation.t)
  | Subset(Operation.t, Operation.t);

let repr = (~sp: string=" ", test: t): Aba.t(string, Operation.t) => {
  let op = labels =>
    labels
    |> List.map(s => sp ++ s ++ sp)
    |> ListUtil.map_first(s => String.trim(s))
    |> ListUtil.map_last(s => String.trim(s));
  let bin = (labels: list(string)) => op([""] @ labels @ [""]);
  let bin_sg = (label: string) => bin([label]);
  switch (test) {
  | Eq(a, b) => ("=" |> bin_sg, [a, b])
  | NotEq(a, b) => ("≠" |> bin_sg, [a, b])
  | Lt(a, b) => ("<" |> bin_sg, [a, b])
  | NotLt(a, b) => ("≥" |> bin_sg, [a, b])
  | Gt(a, b) => (">" |> bin_sg, [a, b])
  | NotGt(a, b) => ("≤" |> bin_sg, [a, b])
  | Mem(p, ctx) => ("∈" |> bin_sg, [p, ctx])
  | Subset(a, b) => ("⊆" |> bin_sg, [a, b])
  };
};

let show = syntax =>
  syntax |> repr |> Aba.join(Fun.id, Operation.show) |> String.concat("");

let show_linked = (map: RuleSpec.map, test: t) =>
  test
  |> repr
  |> (
    ((labels, ops) as aba) =>
      switch (test, ops) {
      | (Eq(Get(_), _), [a, b]) => (labels, [a, b])
      | _ => aba
      }
  )
  |> Aba.join(Fun.id, Operation.show_linked(map))
  |> String.concat("");

let get_symbols: t => list(string) =
  fun
  | Eq(a, b)
  | NotEq(a, b)
  | Lt(a, b)
  | NotLt(a, b)
  | Gt(a, b)
  | NotGt(a, b)
  | Mem(a, b)
  | Subset(a, b) => Operation.get_symbols(a) @ Operation.get_symbols(b);

let go: (RuleSpec.map, t) => bool =
  (map, test) => {
    let go_op = Operation.go(map);
    switch (test) {
    | Eq(a, b) => DrvSyntax.eq(go_op(a), go_op(b))
    | NotEq(a, b) => DrvSyntax.eq(go_op(a), go_op(b))
    | Lt(a, b) =>
      let a = go_op(a) |> int_of_numlit;
      let b = go_op(b) |> int_of_numlit;
      a < b;
    | NotLt(a, b) =>
      let a = go_op(a) |> int_of_numlit;
      let b = go_op(b) |> int_of_numlit;
      a >= b;
    | Gt(a, b) =>
      let a = go_op(a) |> int_of_numlit;
      let b = go_op(b) |> int_of_numlit;
      a > b;
    | NotGt(a, b) =>
      let a = go_op(a) |> int_of_numlit;
      let b = go_op(b) |> int_of_numlit;
      a <= b;
    | Mem(p, ctx) =>
      let p = go_op(p);
      let ctx = go_op(ctx) |> list_of_ctx;
      DrvSyntax.mem_ctx(p, ctx);
    | Subset(a, b) =>
      let a = go_op(a) |> list_of_ctx;
      let b = go_op(b) |> list_of_ctx;
      DrvSyntax.subset_ctx(a, b);
    };
  };

let of_tests: Rule.t => list(t) = {
  module SymbolMap =
    SymbolMap.M({
      type target = Operation.t;
      let f: string => target = s => Get(s);
    });
  SymbolMap.(
    fun
    // Type Validity
    | TV_Num
    | TV_Bool
    | TV_Unit
    | TV_Arrow
    | TV_Prod
    | TV_Sum => []
    | TV_Rec => [Eq(delta', Cons(Type(TVarOfTPat(tpat)), delta))]
    | TV_TVar => [Mem(Type(t), delta)] // TODO
    // Typing
    | A_Subsumption => []
    | T_True
    | S_True => []
    | T_False
    | S_False => []
    | T_If
    | S_If
    | A_If => []
    | T_Num // TODO
    | S_Num => [] // TODO
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
    | T_Var // TODO
    | S_Var => [Mem(HasType(x, t), gamma)] // TODO
    | T_LetAnn_TV => [
        Eq(gamma', Cons(HasType(VarOfPat(x), t_def), gamma)),
        Subset(delta, gamma),
      ]
    | T_LetAnn
    | S_LetAnn
    | A_LetAnn
    | T_Let
    | S_Let
    | A_Let => [Eq(gamma', Cons(HasType(VarOfPat(x), t_def), gamma))]
    | T_FunAnn_TV => [
        Eq(gamma', Cons(HasType(VarOfPat(x), t_in), gamma)),
        Subset(delta, gamma),
      ]
    | T_FunAnn
    | S_FunAnn
    | A_FunAnn
    | T_Fun
    | A_Fun => [Eq(gamma', Cons(HasType(VarOfPat(x), t_in), gamma))]
    | T_Ap
    | S_Ap => []
    | T_Triv
    | S_Triv => []
    | T_Pair
    | S_Pair
    | A_Pair => []
    | T_LetPair
    | S_LetPair
    | A_LetPair => [
        Eq(
          gamma',
          Cons(
            HasType(VarOfPat(y), t2),
            Cons(HasType(VarOfPat(x), t1), gamma),
          ),
        ),
      ]
    | T_PrjL
    | S_PrjL
    | T_PrjR
    | S_PrjR => []
    | T_InjL
    | A_InjL
    | T_InjR
    | A_InjR => []
    | T_Case
    | S_Case
    | A_Case => [
        Eq(gamma', Cons(HasType(VarOfPat(x), t1), gamma)),
        Eq(gamma'', Cons(HasType(VarOfPat(y), t2), gamma)),
      ]
    | T_FixAnn_TV => [
        Eq(gamma', Cons(HasType(VarOfPat(x), t), gamma)),
        Subset(delta, gamma),
      ]
    | T_FixAnn
    | T_Fix => [Eq(gamma', Cons(HasType(VarOfPat(x), t), gamma))]
    | T_Roll
    | T_Unroll => [
        Eq(t_body', SubstTy((Rec(tpat, t_body), tpat), t_body)),
      ]
    // Evaluation
    | E_If_T => []
    | E_If_F => []
    | E_Neg => [Eq(n', Neg(n))]
    | E_Plus => [Eq(n', Plus(n1, n2))]
    | E_Minus => [Eq(n', Minus(n1, n2))]
    | E_Times => [Eq(n', Times(n1, n2))]
    | E_Lt_T => [Lt(n1, n2)]
    | E_Lt_F => [NotLt(n1, n2)]
    | E_Gt_T => [Gt(n1, n2)]
    | E_Gt_F => [NotGt(n1, n2)]
    | E_Eq_T => []
    | E_Eq_F => [NotEq(n1, n2)]
    | E_Let => [Eq(e_body', Subst((v_def, x), e_body))]
    | E_Ap => [Eq(e_body', Subst((v2, x), e_body))]
    | E_Pair => []
    | E_LetPair => [Eq(e_body', Subst((v1, x), Subst((v2, y), e_body)))]
    | E_PrjL => []
    | E_PrjR => []
    | E_InjL => []
    | E_InjR => []
    | E_Case_L => [Eq(e1', Subst((v, x), e1))]
    | E_Case_R => [Eq(e2', Subst((v, y), e2))]
    | E_Fix => [Eq(e', Subst((Fix(x, e_body), x), e_body))]
    | E_Roll => []
    | E_Unroll => []
    | E_Val => []
    // Values
    | V_True
    | V_False
    | V_Num // TODO
    | V_Fun
    | V_Triv
    | V_Pair
    | V_InjL
    | V_InjR
    | V_Roll => []
    // Logical Proposition
    | Assumption => [Mem(a, gamma)]
    | And_I => []
    | And_E_L => []
    | And_E_R => []
    | Or_I_L => []
    | Or_I_R => []
    | Or_E => [Eq(gamma', Cons(a, gamma)), Eq(gamma'', Cons(b, gamma))]
    | Implies_I => [Eq(gamma', Cons(a, gamma))]
    | Implies_E => []
    | Truth_I => []
    | Falsity_E => []
  );
};

// module Unbox = {
//   // Note (Zhiyao): This module is responsible for unboxing syntax elements with
//   // primitive types, such as NumLit(int) and Var(string). We handle unboxing in
//   // this separate module to avoid using GADTs in the main module.
//   //
//   // However, in RuleSpec, we have already checked the syntax cls, so (suppose
//   // our implementation is correct) we should not raise any exception here. We
//   // are simply unboxing the syntax to its primitive type.

//   type t(_) =
//     | Ctx: t(list(DrvSyntax.t))
//     | NumLit: t(int)
//     | Var: t(string)
//     | TVar: t(string)
//     | Pat: t(string)
//     | TPat: t(string)
//     | Atom: t(string);

//   let rec go: type a. (DrvSyntax.t, t(a)) => result(a, RuleSpec.failure) =
//     (syntax, op) => {
//       switch ((op, DrvSyntax.term_of(syntax))) {
//       | (Ctx, Ctx(ctx)) => Ok(ctx)
//       | (NumLit, NumLit(n)) => Ok(n)
//       | (Var, Var(s)) => Ok(s)
//       | (TVar, TVar(s)) => Ok(s)
//       | (Pat, Pat(s)) => Ok(s)
//       | (TPat, TPat(s)) => Ok(s)
//       | (Atom, Atom(s)) => Ok(s)
//       | (Ctx, _)
//       | (NumLit, _)
//       | (Var, _)
//       | (TVar, _)
//       | (Pat, _)
//       | (TPat, _)
//       | (Atom, _) => failwith("RuleTest.Unbox.go: cannot unbox")
//       };
//     };
// };

// module Operation = {
//   // [@deriving (show({with_path: false}), sexp, yojson)]

//   type specced = RuleSpec.specced;

//   type t(_) =
//     // Get what has been registered
//     | Unbox(Unbox.t('a), SymbolMap.key): t('a)
//     | Get(string): t(syntax)
//     // Reconstructing
//     | Type(t(syntax)): t(syntax)
//     | HasType(t(syntax), t(syntax)): t(syntax)
//     | Fix(t(syntax), t(syntax)): t(syntax)
//     | Rec(t(syntax), t(syntax)): t(syntax)
//     | Var(t(string)): t(syntax)
//     | TVar(t(string)): t(syntax)
//     | NumLit(t(int)): t(syntax)
//     // Operations
//     | Subst((t(syntax), t(string)), t(syntax)): t(syntax)
//     | SubstTy((t(syntax), t(string)), t(syntax)): t(syntax)
//     | Cons(t(syntax), t(list(syntax))): t(syntax)
//     | Neg(t(int)): t(int)
//     | Plus(t(int), t(int)): t(int)
//     | Minus(t(int), t(int)): t(int)
//     | Times(t(int), t(int)): t(int);

//   type failure =
//     | FailUnbox(RuleSpec.specced)
//     | NotReg; // No chance to show in frontend

//   let rec go: type a. (RuleSpec.map, t(a)) => result(a, failure) =
//     (map, op) => {
//       let (let$) = (x, f) =>
//         switch (x) {
//         | Ok(x) => f(x)
//         | Error(e) => Error(e)
//         };
//       let go = go(map);
//       switch (op) {
//       | Get(s) =>
//         switch (RuleSpec.Map.find_opt(s, map)) {
//         | Some((_, syntax)) => Ok(syntax)
//         | None => Error(NotReg)
//         }
//       | D_Ctx(a) =>
//         let$ syntax = go(a);
//         switch (DrvSyntax.term_of(syntax)) {
//         | Ctx(ctx) => Ok(ctx)
//         | _ => Error("RuleTest.go: not a context")
//         };
//       | D_NumLit(a) =>
//         let$ syntax = go(a);
//         switch (DrvSyntax.term_of(syntax)) {
//         | NumLit(n) => Ok(n)
//         | _ => Error("RuleTest.go: not a number")
//         };
//       | _ => failwith("RuleTest.go: not implemented")
//       };
//     };
