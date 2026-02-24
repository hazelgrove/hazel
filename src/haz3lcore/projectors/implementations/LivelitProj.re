open Util;
open Virtual_dom.Vdom;
open ProjectorBase;
open Language;

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = unit;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = unit;

  let get_model = (info: info) =>
    switch (info.statics) {
    | Some(
        InfoExp({
          term: {term: Ap(_dir, {term: LivelitName(llname), _}, model), _},
          _,
        }),
      ) =>
      Some((llname, model))
    | _ => None
    };

  let init = (any: Language.Any.t) =>
    switch (any) {
    | Exp({term: Ap(_dir, {term: LivelitName(_), _}, _), _})
    | Exp({
        term: Parens({term: Ap(_dir, {term: LivelitName(_), _}, _), _}),
        _,
      }) =>
      Some()
    | _ => None
    };

  let placeholder = (_model, info) => {
    switch (get_model(info), info.statics) {
    | (Some((llname, _)), Some(InfoExp(exp))) =>
      /* Get the livelit size */
      switch (Ctx.lookup_livelit(exp.ctx, llname)) {
      | Some(ll) => ll.size
      | None =>
        /* Default size */
        ProjectorCore.Shape.inline(32)
      }
    | _ =>
      /* Default size */
      ProjectorCore.Shape.inline(32)
    };
  };

  let replace_model_term =
      (updated_model_term: TermBase.Exp.t, start_term: TermBase.Any.t)
      : TermBase.Any.t =>
    switch (start_term) {
    | Exp({term: Ap(Forward, name, _model), _} as rest) =>
      Exp({
        ...rest,
        term: Ap(Forward, name, updated_model_term),
      })
    | _ =>
      print_endline("Warning - LivelitProj.replace_model_term: not an Ap");
      start_term;
    };
  let update = (_model, _info, action) =>
    switch (action) {
    | _ => print_endline("Warning - LivelitProj.update: No action")
    };

  let focus_pointer = (id: Id.t) => {
    JsUtil.get_elem_by_id(Id.cls(id))##focus;
  };

  let focusable =
    Focusable.{
      pointer: Some(focus_pointer),
      keyboard: None,
    };

  let dynamics = false;

  let view = ({info, parent, _}: View.args(model, action)) => {
    let ctx =
      switch (info.statics) {
      | Some(InfoExp(exp)) => exp.ctx
      | _ => Ctx.empty
      };

    let node =
      switch (get_model(info)) {
      | Some((ll_name, model)) =>
        let ll = Ctx.lookup_livelit(ctx, ll_name);

        switch (ll) {
        | Some(ll) =>
          let action_callback = (action: LivelitCtx.action_exp) => {
            let new_model = ll.update(action, model);

            let updated_segment =
              info.utility.lift_syntax(
                replace_model_term(new_model),
                Inline.Block,
                info.syntax,
              );

            switch (updated_segment) {
            | Some(s) => parent(SetSyntax(s))
            | None =>
              print_endline("Warning - LivelitProj.view: lift_syntax failed");
              Ui_effect.Ignore;
            };
          };

          let list_contents = ll.view(model, action_callback);
          Node.div(
            ~attrs=[Attr.class_(ll_name), Attr.id(Id.cls(info.id))],
            [list_contents],
          );
        | None =>
          print_endline("Warning - LivelitProj.view: not found in context");
          Node.text("No livelit found");
        };
      | None =>
        print_endline("Warning - LivelitProj.view: get is empty");
        Node.text("No livelit found");
      };

    View.mk(node);
  };
};
