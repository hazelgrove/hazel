module Vdom = Virtual_dom.Vdom;
open Vdom;
open OptUtil.Syntax;

type assistant_action_categories =
  | InsertVar
  | InsertApp;

type assistant_action = {
  category: assistant_action_categories,
  // view: actually, calc this from action uhexp
  text: string, // what to type to autocomplete
  action: Action.t,
};

let mk_var = name =>
  Seq.mk(UHExp.var(name), []) |> UHExp.mk_OpSeq |> UHExp.Block.wrap';

let get_hole_number = (t: CursorInfo.cursor_term): MetaVar.t => {
  switch (t) {
  | Exp(_, EmptyHole(n)) => n
  | _ => failwith("get_hole_number: Not on hole")
  };
};

let mk_var_action =
    (t: CursorInfo.cursor_term, name: string): assistant_action => {
  {
    category: InsertVar,
    text: name,
    action: FillExpHole(get_hole_number(t), mk_var(name)),
  };
};

let collate_insert_var_actions =
    (t: CursorInfo.cursor_term, env: VarCtx.t): list(assistant_action) => {
  List.map(((name, _)) => mk_var_action(t, name), env);
};

let wrap_space = (operator, seq) =>
  Seq.S(operator, A(Operators_Exp.Space, seq));

let rec mk_ap_seq_holes =
        (f_ty: HTyp.t, hole_ty: HTyp.t)
        : option(Seq.t(UHExp.operand, UHExp.operator)) => {
  switch (f_ty) {
  | Arrow(_, output_ty) =>
    if (HTyp.consistent(output_ty, hole_ty)) {
      Some(S(EmptyHole(0), E));
    } else {
      let+ affix = mk_ap_seq_holes(output_ty, hole_ty);
      wrap_space(UHExp.EmptyHole(0), affix);
    }
  | _ => None
  };
};

let mk_ap =
    (ctx, u_gen, f: Var.t, f_ty: HTyp.t, hole_ty: HTyp.t): option(UHExp.t) => {
  let+ inner_seq = mk_ap_seq_holes(f_ty, hole_ty);
  wrap_space(UHExp.var(f), inner_seq)
  |> UHExp.mk_OpSeq
  |> UHExp.Block.wrap'
  |> Statics_Exp.syn_fix_holes(ctx, u_gen, ~renumber_empty_holes=true)
  |> (((x, _, _)) => x);
};

let mk_app_action =
    (
      ctx,
      u_gen,
      t: CursorInfo.cursor_term,
      f_name: string,
      f_ty: HTyp.t,
      hole_ty: HTyp.t,
    )
    : assistant_action => {
  // what should behavior be if hole_ty is Hole?
  // Right now it suggests a single-arg app
  // possiblities: no suggestion vs. all options vs. fully applied
  let e =
    mk_ap(ctx, u_gen, f_name, f_ty, hole_ty)
    |> OptUtil.get(_ => failwith("mk_app_action"));
  {
    category: InsertApp,
    text: f_name,
    action: FillExpHole(get_hole_number(t), e),
  };
};

//copied
let code_node = text =>
  Vdom.Node.div(
    [Vdom.Attr.classes(["code-font"])],
    [Vdom.Node.text(text)],
  );

let list_vars_view = (inject, t, vars: VarCtx.t) => {
  VarMap.map(
    ((var, ty)) => {
      let {action, _} = mk_var_action(t, var);
      Node.div(
        [
          Attr.classes(["option"]),
          Attr.on_click(_ => {
            Event.Many([
              Event.Prevent_default,
              Event.Stop_propagation,
              inject(ModelAction.AcceptSuggestion(action)),
            ])
          }),
        ],
        [code_node(var), Node.text(" : "), HTypCode.view(ty)],
      );
    },
    vars,
  )
  |> List.map(((_, b)) => b);
};

let list_fns_view = (ctx, u_gen, inject, t, hole_ty, fn_vars: VarCtx.t) => {
  VarMap.map(
    ((var, ty)) => {
      let {action, _} = mk_app_action(ctx, u_gen, t, var, ty, hole_ty);
      Node.div(
        [
          Attr.classes(["option"]),
          Attr.on_click(_ => {
            Event.Many([
              Event.Prevent_default,
              Event.Stop_propagation,
              inject(ModelAction.AcceptSuggestion(action)),
            ])
          }),
        ],
        [code_node(var), Node.text(" : "), HTypCode.view(ty)],
      );
    },
    fn_vars,
  )
  |> List.map(((_, b)) => b);
};

let view =
    (
      ~inject: ModelAction.t => Vdom.Event.t,
      //settings: Settings.CursorInspector.t,
      {ctx, cursor_term, _} as cursor_info: CursorInfo.t,
    )
    : Vdom.Node.t => {
  let ty =
    OptUtil.get(
      () => failwith("assistant view"),
      StrategyGuide.get_type(cursor_info),
    );
  let env = StrategyGuide.extract_vars(ctx, ty);
  let fn_env = StrategyGuide.fun_vars(ctx, ty);

  let u_gen = 0; // TODO: get u_gen from somewhere
  Node.div(
    [Attr.classes(["type-driven"])],
    list_fns_view(ctx, u_gen, inject, cursor_term, ty, fn_env)
    @ list_vars_view(inject, cursor_term, env),
  );
};
