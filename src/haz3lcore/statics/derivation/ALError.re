// open Util;
  // open AL;
  // open ErrorCommon;
  // module ALError = {
  //   [@deriving (show({with_path: false}), sexp, yojson)]
  //   type t =
  //     | PremiseMismatch(int, int) /* expected, actual */
  //     | NotAnalized(Syntax_AL.cls, bind(Syntax_AL.t))
  //     | MisMatch(mismatch)
  //     | NotEqual(notequal)
  //   and mismatch =
  //     | Value(Value.cls, bind(Value.t)) /* expected, actual */
  //     | Expr(Expr.cls, bind(Expr.t))
  //     | Judgement(Judgement.cls, bind(Judgement.t))
  //   and notequal =
  //     | Value(bind(Value.t), bind(Value.t)) /* expected, actual */
  //     | Expr(bind(Expr.t), bind(Expr.t));
  // };
  // module Value = {
  //   include Value;
  //   type unbox_req('a) =
  //     | NumLit: unbox_req(bind(int));
  //   let unbox: type a. (unbox_req(a), bind(t)) => result(a, ALError.t) =
  //     (req, p) => {
  //       switch (req, p.value) {
  //       | (NumLit, NumLit(n)) => Ok(n |> copy_pos(p))
  //       };
  //     };
  //   let expect_eq = (a: bind(t), b: bind(t)): result(unit, ALError.t) =>
  //     if (eq(a.value, b.value)) {
  //       Ok();
  //     } else {
  //       Error(NotEqual(Value(a, b)));
  //     };
  // };
  // module Expr = {
  //   include Expr;
  //   type unbox_req('a) =
  //     | NumLit: unbox_req(bind(int))
  //     | UnOp: unbox_req((bind(unop), bind(t)))
  //     | BinOp: unbox_req((bind(binop), bind(t), bind(t)));
  //   let unbox: type a. (unbox_req(a), bind(t)) => result(a, ALError.t) =
  //     (req, p) => {
  //       let mk_error = (cls: cls): result(a, ALError.t) =>
  //         Error(MisMatch(Expr(cls, p)));
  //       switch (req, p.value) {
  //       | (NumLit, NumLit(n)) => Ok(n |> copy_pos(p))
  //       | (UnOp, UnOp(op, a)) => Ok((op, a) |> copy_pos2(p))
  //       | (BinOp, BinOp(op, a, b)) => Ok((op, a, b) |> copy_pos3(p))
  //       | (NumLit, _) => mk_error(NumLit)
  //       | (UnOp, _) => mk_error(UnOp)
  //       | (BinOp, _) => mk_error(BinOp)
  //       };
  //     };
  //   let expect_eq = (a: bind(t), b: bind(t)): result(unit, ALError.t) =>
  //     if (eq(a.value, b.value)) {
  //       Ok();
  //     } else {
  //       Error(NotEqual(Expr(a, b)));
  //     };
  // };
  // module Judgement = {
  //   include Judgement;
  //   type unbox_req('a) =
  //     | Value: unbox_req(bind(Expr.t))
  //     | Eval: unbox_req((bind(Expr.t), bind(Value.t)));
  //   let unbox: type a. (unbox_req(a), bind(t)) => result(a, ALError.t) =
  //     (req, p) => {
  //       let mk_error = (cls: cls): result(a, ALError.t) =>
  //         Error(MisMatch(Judgement(cls, p)));
  //       switch (req, p.value) {
  //       | (Value, Value(e)) => Ok(e |> copy_pos(p))
  //       | (Eval, Eval(e, v)) => Ok((e, v) |> copy_pos2(p))
  //       | (Value, _) => mk_error(Value)
  //       | (Eval, _) => mk_error(Eval)
  //       };
  //     };
  // };
  // module Syntax_AL = {
  //   include Syntax_AL;
  //   type unbox_req('a) =
  //     | Value: unbox_req(bind(Value.t))
  //     | Expr: unbox_req(bind(Expr.t))
  //     | Judgement: unbox_req(bind(Judgement.t));
  //   let unbox: type a. (unbox_req(a), bind(t)) => result(a, ALError.t) =
  //     (req, p) => {
  //       let mk_error = (cls: cls): result(a, ALError.t) =>
  //         Error(NotAnalized(cls, p));
  //       switch (req, p.value) {
  //       | (Value, Value(e)) => Ok(e |> copy_pos(p))
  //       | (Value, _) => mk_error(Value)
  //       | (Expr, Expr(e)) => Ok(e |> copy_pos(p))
  //       | (Expr, _) => mk_error(Expr)
  //       | (Judgement, Judgement(e)) => Ok(e |> copy_pos(p))
  //       | (Judgement, _) => mk_error(Judgement)
  //       };
  //     };
  // };
  // module Premises = {
  //   open Syntax_AL;
  //   let expect_num =
  //       (n: int, prems: list(t)): result(int => bind(t), ALError.t) =>
  //     if (List.length(prems) == n) {
  //       Ok(i => {pos: Prems(i), value: List.nth(prems, i)});
  //     } else {
  //       Error(PremiseMismatch(n, List.length(prems)));
  //     };
  // };
  // let verify =
  //     (rule: Rule.t, concl: Syntax_AL.t, prems: list(Syntax_AL.t))
  //     : option(ALError.t) => {
  //   let (let$) = (x, f) =>
  //     switch (x) {
  //     | Ok(x) => f(x)
  //     | Error(e) => Some(e)
  //     };
  //   let$ concl = Ok({pos: Concl, value: concl});
  //   let$ prems = Premises.expect_num(Rule.prems_num(rule), prems);
  //   ignore(prems);
  //   switch (rule) {
  //   | V_NumLit =>
  //     let$ j = Syntax_AL.unbox(Judgement, concl);
  //     let$ e = Judgement.unbox(Value, j);
  //     let$ _ = Expr.unbox(NumLit, e);
  //     None;
  //   // TODO: (refactor)
  //   | _ => None
  //   // | E_NumLit =>
  //   // | E_Neg =>
  //   // | E_Plus =>
  //   // | E_Minus =>
  //   // | E_Times =>
  //   };
  // };
