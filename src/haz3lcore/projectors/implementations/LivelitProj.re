open Util;
open Virtual_dom.Vdom;
open ProjectorInterface;
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

  let mk =
      (any: Term.Any.t, _ed: unit => option(Editor.model))
      : option(model(Editor.model)) =>
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

  let dynamics = false;

  /* Placeholder implementation */
  let placeholder = (~common, ~id, model) => {
    let statics = Statics.Map.lookup(id, common.statics.info_map);

    let ctx =
      switch (statics) {
      | Some(InfoExp(exp)) => exp.ctx
      | _ => Ctx.empty
      };

    let ll = Ctx.lookup_livelit(ctx, model.livelit_name);
    switch (ll) {
    | Some(ll) => ll.size
    | None =>
      /* Default size */
      ProjectorShape.inline(32)
    };
  };

  /* Update implementation */
  let update = (~common, ~sort as _, ~id, model, action) => {
    let ctx =
      switch (Statics.Map.lookup(id, common.statics.info_map)) {
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
  };

  /* Make term implementation */
  let mk_term =
      (~sort as _, ~prev as _, model): (model(Editor.model), Calc.t(Any.t)) => (
    model,
    Calc.NewValue(Exp(model.model)),
  );

  /* Calculate implementation */
  let calculate = (~common as _, model) => model;

  /* Cursor info implementation */
  let get_cursor_info =
      (~common as _, ~inject as _, ~read_only as _, _model, _focus) => Cursor.empty;

  /* View implementation */
  let view =
      (
        ~common,
        ~inject,
        ~escape as _,
        ~take_focus as _,
        ~focus as _,
        ~id,
        model,
      ) => {
    let ctx =
      switch (Statics.Map.lookup(id, common.statics.info_map)) {
      | Some(InfoExp(exp)) => exp.ctx
      | _ => Ctx.empty
      };
    let ll = Ctx.lookup_livelit(ctx, model.livelit_name);

    let node =
      switch (ll) {
      | Some(ll) =>
        let action_callback = action => inject(action);
        let list_contents = ll.view(model.model, action_callback);
        Node.div(
          ~attrs=[Attr.class_(model.livelit_name), Attr.id(Id.cls(id))],
          [list_contents],
        );
      | None =>
        Node.div(
          ~attrs=[Attr.classes(["missing-livelit"])],
          [Node.text("Missing: " ++ model.livelit_name)],
        )
      };

    View.mk(node);
  };
};
