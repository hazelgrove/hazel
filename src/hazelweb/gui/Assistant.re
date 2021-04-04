module Vdom = Virtual_dom.Vdom;
open Vdom;
open OptUtil.Syntax;

type assistant_action_categories =
  | InsertVar
  | InsertApp;

let action_abbrev =
  fun
  | InsertVar => "VAR"
  | InsertApp => "APP";

type assistant_action = {
  category: assistant_action_categories,
  text: string,
  action: Action.t,
  result: UHExp.t,
  res_ty: HTyp.t,
};

let mk_var = name =>
  Seq.mk(UHExp.var(name), []) |> UHExp.mk_OpSeq |> UHExp.Block.wrap';

let get_hole_number = (t: CursorInfo.cursor_term): MetaVar.t => {
  switch (t) {
  | Exp(_, EmptyHole(n)) => n
  | _ => failwith("get_hole_number: Not on hole")
  };
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
    (
      {ty, ctx, u_gen, _}: AssistantCommon.cursor_info_pro,
      f: Var.t,
      f_ty: HTyp.t,
    )
    : option(UHExp.t) => {
  let+ inner_seq = mk_ap_seq_holes(f_ty, ty);
  wrap_space(UHExp.var(f), inner_seq)
  |> UHExp.mk_OpSeq
  |> UHExp.Block.wrap'
  |> Statics_Exp.syn_fix_holes(ctx, u_gen, ~renumber_empty_holes=true)
  |> (((x, _, _)) => x);
};

let mk_var_action =
    ({term, _}: AssistantCommon.cursor_info_pro, name: string, ty: HTyp.t)
    : assistant_action => {
  let e = mk_var(name);
  {
    category: InsertVar,
    text: name,
    result: e,
    res_ty: ty,
    action: FillExpHole(get_hole_number(term), e),
  };
};

let mk_app_action =
    (cursor: AssistantCommon.cursor_info_pro, name: string, ty: HTyp.t)
    : assistant_action => {
  let e =
    mk_ap(cursor, name, ty) |> OptUtil.get(_ => failwith("mk_app_action"));
  {
    category: InsertApp,
    text: name,
    action: FillExpHole(get_hole_number(cursor.term), e),
    res_ty: ty,
    result: e,
  };
};

let compute_actions =
    ({mode, ctx, ty, _} as cursor: AssistantCommon.cursor_info_pro)
    : list(assistant_action) => {
  let vars =
    switch (mode) {
    | Synthetic =>
      AssistantCommon.extract_vars(ctx, HTyp.Hole)
      |> List.map(((name, var_ty)) => mk_var_action(cursor, name, var_ty))
    | Analytic =>
      AssistantCommon.extract_vars(ctx, ty)
      |> List.map(((name, var_ty)) => mk_var_action(cursor, name, var_ty))
    };
  let apps =
    switch (mode) {
    | Synthetic => []
    | Analytic =>
      AssistantCommon.fun_vars(ctx, ty)
      |> List.map(((name, f_ty)) => mk_app_action(cursor, name, f_ty))
    };
  vars @ apps;
};

let action_view = (inject, font_metrics, act: assistant_action) => {
  let {action, result, res_ty, _} = act;
  let width = 80; //TODO: unhardcode?
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
    UHCode.codebox_view(~font_metrics, width, result)
    @ [Node.text(" : "), HTypCode.view(res_ty)],
  );
};

let view =
    (
      ~inject: ModelAction.t => Vdom.Event.t,
      ~font_metrics: FontMetrics.t,
      cursor_info: CursorInfo.t,
      u_gen: MetaVarGen.t,
    )
    : Vdom.Node.t => {
  let cursor = AssistantCommon.promote_cursor_info(cursor_info, u_gen);
  let actions = compute_actions(cursor);
  let action_views = List.map(action_view(inject, font_metrics), actions);
  Node.div([Attr.classes(["type-driven"])], action_views);
};
