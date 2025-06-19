open Util;
open Virtual_dom.Vdom;
open ProjectorBase;
open Language;

[@deriving (show({with_path: false}), sexp, yojson)]
type model('ed) = {
  livelit_name: string,
  model: Exp.t,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type action('ed_a) = LivelitCtx.action_exp;

[@deriving (show({with_path: false}), sexp, yojson)]
type focus('ed_f) = unit;

let methods:
  methods(model('ed_m), action('ed_a), focus('ed_f), 'ed_m, 'ed_a, 'ed_f) = {
  init: (~copy_ed as _, any: TermBase.Any.t, _ed) => {
    print_endline("LivelitProj.init");
    switch (any) {
    | Exp({term: Ap(Forward, {term: LivelitName(ll), _}, model), _})
    | Exp({
        term:
          Parens({term: Ap(Forward, {term: LivelitName(ll), _}, model), _}),
        _,
      }) =>
      Some({
        livelit_name: ll,
        model,
      })
    | _ => None
    };
  },
  dynamics: false,
  update: (~update_ed as _, ~common as _, ~sort as _, info, model, action) => {
    let ctx =
      switch (info.statics) {
      | Some(InfoExp(exp)) => exp.ctx
      | _ => Ctx.empty
      };
    let ll = Ctx.lookup_livelit(ctx, model.livelit_name);
    switch (ll) {
    | Some(ll) => {
        ...model,
        model: ll.update(action, model.model),
      }
    | None => model
    };
  },
  view:
    (
      ~common as _,
      ~ed_str as _,
      ~view_ed as _,
      ~view_editable as _,
      ~enter_ed as _,
      ~mk_ed as _,
      ~mk_term_ed as _,
      ~calculate_ed as _,
      ~local,
      ~parent as _,
      ~focus as _,
      ~focussed as _,
      {livelit_name, model},
      info,
    ) => {
    let ctx =
      switch (info.statics) {
      | Some(InfoExp(exp)) => exp.ctx
      | _ => Ctx.empty
      };
    let node = {
      let ll = Ctx.lookup_livelit(ctx, livelit_name);
      switch (ll) {
      | Some(ll) =>
        let action_callback = (action: LivelitCtx.action_exp) => {
          local(action);
        };
        let list_contents = ll.view(model, action_callback);
        Node.div(
          ~attrs=[Attr.class_(livelit_name), Attr.id(Id.cls(info.id))],
          [list_contents],
        );
      | None =>
        print_endline("Warning - LivelitProj.view: not found in context");
        Node.text("No livelit found");
      };
    };
    View.mk(node);
  },
  placeholder: (~ed_size as _, {livelit_name, _}, info) => {
    let ctx =
      switch (info.statics) {
      | Some(InfoExp(exp)) => exp.ctx
      | _ => Ctx.empty
      };

    let ll = Ctx.lookup_livelit(ctx, livelit_name);
    switch (ll) {
    | Some(ll) => ll.size
    | None =>
      /* Default size */
      ProjectorShape.inline(32)
    };
  },
  mk_term: (~mk_term_ed as _, ~sort as _, ~prev as _, model) => (
    model,
    Calc.NewValue(Exp(model.model)),
  ),
  calculate: (~calculate_ed as _, ~common as _, m) => m,
  get_cursor_info:
    (
      ~get_cursor_info_ed as _,
      ~common as _,
      ~inject as _: action('a) => Ui_effect.t(unit),
      ~read_only as _,
      _model,
      _focus,
    ) => Cursor.empty,
  sexp_of_model,
  model_of_sexp,
  yojson_of_model,
  model_of_yojson,
  sexp_of_action,
  action_of_sexp,
  yojson_of_action,
  action_of_yojson,
  sexp_of_focus,
  focus_of_sexp,
  yojson_of_focus,
  focus_of_yojson,
};

module M =
       (Editor: ProjectorInterface.EDITOR)

         : (
           ProjectorInterface.PROJECTOR with
             type model' = model(Editor.model) and
             type action' = action(Editor.action) and
             type focus' = focus(Editor.focus) and
             type editor_model = Editor.model
       ) => {
  type editor_model = Editor.model;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type model' = model(Editor.model);
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action' = action(Editor.action);
  [@deriving (show({with_path: false}), sexp, yojson)]
  type focus' = focus(Editor.focus);

  let init = (any, e) => methods.init(~copy_ed=Editor.Model.copy, any, e);
  let dynamics = false;
  let placeholder = (model, info) =>
    methods.placeholder(~ed_size=Editor.View.get_dimensions, model, info);

  let update = (~common, ~sort, info, model, action) =>
    methods.update(
      ~update_ed=Editor.Update.update,
      ~common,
      ~sort,
      info,
      model,
      action,
    );

  let mk_term = (~sort, ~prev, model) =>
    methods.mk_term(~mk_term_ed=Editor.Update.make_term, ~sort, ~prev, model);

  let calculate = (~common, model) =>
    methods.calculate(~calculate_ed=Editor.Update.calculate, ~common, model);

  let get_cursor_info = (~common, ~inject, ~read_only, model, focus) =>
    methods.get_cursor_info(
      ~get_cursor_info_ed=Editor.Focus.get_cursor_info,
      ~common,
      ~inject,
      ~read_only,
      model,
      focus,
    );

  let view = (~common, ~local, ~parent, ~focus, ~focussed, model, info) =>
    methods.view(
      ~common,
      ~ed_str=Editor.View.print_string,
      ~view_ed=
        Editor.View.view(
          ~font_metrics=common.font_metrics,
          ~secondary_icons=common.secondary_icons,
        ),
      ~view_editable=Editor.View.view_editable,
      ~enter_ed=Editor.Focus.enter,
      ~mk_ed=Editor.Model.mk,
      ~mk_term_ed=Editor.Update.make_term,
      ~calculate_ed=Editor.Update.calculate,
      ~local,
      ~parent,
      ~focus,
      ~focussed,
      model,
      info,
    );
};
