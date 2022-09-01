open Virtual_dom.Vdom;
open Node;
open Util.Web;

let cls_str = (ci: Core.Statics.t): string =>
  switch (ci) {
  | Invalid(msg) => Core.Term.show_parse_flag(msg)
  | InfoExp({cls, _}) => Core.Term.UExp.show_cls(cls)
  | InfoPat({cls, _}) => Core.Term.UPat.show_cls(cls)
  | InfoTyp({cls, _}) => Core.Term.UTyp.show_cls(cls)
  | InfoRul({cls}) => Core.Term.URul.show_cls(cls)
  };

let errorc = "error";
let happyc = "happy";
let infoc = "info";

let error_view = (err: Core.Statics.error) =>
  switch (err) {
  | Multi => div([clss([errorc, "err-multi"])], [text("⑂ Multi Hole")])
  | FreeVariable =>
    div(
      [clss([errorc, "err-free-variable"])],
      [text("⊥ Free Variable")],
    )
  | SynInconsistentBranches(tys) =>
    div(
      [clss([errorc, "err-inconsistent-branches"])],
      [text("≉ Branches:")] @ List.map(Type.view, tys),
    )
  | TypeInconsistent(ty_ana, ty_syn) =>
    div(
      [clss([errorc, "err-type-inconsistent"])],
      [Type.view(ty_ana), text("≉"), Type.view(ty_syn)],
    )
  };

let happy_view = (suc: Core.Statics.happy) => {
  switch (suc) {
  | SynConsistent(ty_syn) =>
    div(
      [clss([happyc, "syn-consistent"])],
      [text("⇒"), Type.view(ty_syn)],
    )
  | AnaConsistent(ty_ana, ty_syn, _ty_join) when ty_ana == ty_syn =>
    div(
      [clss([happyc, "ana-consistent-equal"])],
      [text("⇐"), Type.view(ty_ana)],
    )
  | AnaConsistent(ty_ana, ty_syn, _ty_join) =>
    div(
      [clss([happyc, "ana-consistent"])],
      [text("⇐"), Type.view(ty_ana), text("≈"), Type.view(ty_syn)],
    )
  | AnaInternalInconsistent(ty_ana, _)
  | AnaExternalInconsistent(ty_ana, _) =>
    div(
      [clss([happyc, "ana-consistent-external"])],
      [
        div(
          [clss(["typ-view", "atom"])],
          [text("⇐"), div([clss(["typ-mod"])], [text("☆")])],
        ),
        Type.view(ty_ana),
      ],
    )
  };
};

let status_view = (err: Core.Statics.error_status) => {
  switch (err) {
  | InHole(error) => error_view(error)
  | NotInHole(happy) => happy_view(happy)
  };
};

let term_tag = (is_err, sort) =>
  div(
    [clss(["term-tag", "term-tag-" ++ sort] @ (is_err ? [errorc] : []))],
    [div([clss(["icon"])], [Icons.magnify]), text(sort)],
  );

let view_of_info = (ci: Core.Statics.t): Node.t => {
  let is_err = Core.Statics.is_error(ci);
  switch (ci) {
  | Invalid(msg) =>
    div(
      [clss([infoc, "unknown"])],
      [text("🚫 " ++ Core.Term.show_parse_flag(msg))],
    )
  | InfoExp({mode, self, _}) =>
    let error_status = Core.Statics.error_status(mode, self);
    div(
      [clss([infoc, "exp"])],
      [term_tag(is_err, "exp"), status_view(error_status)],
    );
  | InfoPat({mode, self, _}) =>
    let error_status = Core.Statics.error_status(mode, self);
    div(
      [clss([infoc, "pat"])],
      [term_tag(is_err, "pat"), status_view(error_status)],
    );
  | InfoTyp({ty, _}) =>
    let ann = div([clss(["typ-view"])], [text(":")]);
    div(
      [clss([infoc, "typ"])],
      [term_tag(is_err, "typ"), ann, Type.view(ty)],
    );
  | InfoRul(_) =>
    div([clss([infoc, "rul"])], [term_tag(is_err, "rul"), text("Rule")])
  };
};

let cls_view = (ci: Core.Statics.t): Node.t =>
  div([clss(["syntax-class"])], [text(cls_str(ci))]);

let id_view = (id): Node.t =>
  div([clss(["id"])], [text(string_of_int(id + 1))]);

let extra_view = (visible: bool, id: int, ci: Core.Statics.t): Node.t =>
  div(
    [clss(["extra"] @ (visible ? ["visible"] : []))],
    [id_view(id), cls_view(ci)],
  );

let toggle_context_and_print_ci = (~inject: Update.t => 'a, ci, _) => {
  print_endline(Core.Statics.show(ci));
  switch (ci) {
  | InfoPat({mode, self, _})
  | InfoExp({mode, self, _}) =>
    Core.Statics.error_status(mode, self)
    |> Core.Statics.show_error_status
    |> print_endline
  | _ => ()
  };
  inject(Set(ContextInspector));
};

let inspector_view =
    (~inject, ~settings: Model.settings, id: int, ci: Core.Statics.t): Node.t =>
  div(
    [
      clss(
        ["cursor-inspector"] @ [Core.Statics.is_error(ci) ? errorc : happyc],
      ),
      Attr.on_mousedown(toggle_context_and_print_ci(~inject, ci)),
    ],
    [
      extra_view(settings.context_inspector, id, ci),
      view_of_info(ci),
      CtxInspector.inspector_view(~settings, id, ci),
    ],
  );

let view =
    (~inject, ~settings, index': option(int), info_map: Core.Statics.map) => {
  switch (index') {
  | Some(index) =>
    switch (Core.Id.Map.find_opt(index, info_map)) {
    | Some(ci) => inspector_view(~inject, ~settings, index, ci)
    | None =>
      div(
        [clss(["cursor-inspector"])],
        [
          div([clss(["icon"])], [Icons.magnify]),
          text("No CI for Index"),
        ],
      )
    }
  | None =>
    div(
      [clss(["cursor-inspector"])],
      [
        div([clss(["icon"])], [Icons.magnify]),
        text("No Indicated Index"),
      ],
    )
  };
};
