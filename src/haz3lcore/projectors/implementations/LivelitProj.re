open Util;
open Virtual_dom.Vdom;
open ProjectorBase;

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
  init: (any: TermBase.Any.t, _ed) => {
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
  focusable: Focusable.non,
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
      ~mk_ed as _,
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
  placeholder: (~ed_str as _, {livelit_name, _}, info) => {
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
