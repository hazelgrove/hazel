open Virtual_dom.Vdom;
open Node;
open Util.Web;

let cls_str = (ci: Core.Statics.t): string =>
  switch (ci) {
  | Invalid => "No Semantics for Syntax"
  | InfoExp({cls, _}) => Core.Term.UExp.show_cls(cls)
  | InfoPat({cls, _}) => Core.Term.UPat.show_cls(cls)
  | InfoTyp({cls, _}) => Core.Term.UTyp.show_cls(cls)
  };

let errorc = "error";
let happyc = "happy";
let infoc = "info";

let error_view = (err: Core.Statics.error) =>
  switch (err) {
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
      [text("⇐☆"), Type.view(ty_ana)],
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
    [text(sort)],
  );

let view_of_info = (ci: Core.Statics.t): Node.t => {
  let is_err = Core.Statics.is_error(ci);
  switch (ci) {
  | Invalid => div([clss([infoc, "unknown"])], [text("? No Info")])
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
    div([clss([infoc, "typ"])], [term_tag(is_err, "typ"), Type.view(ty)])
  };
};

let cls_view = (ci: Core.Statics.t): Node.t =>
  div([clss(["syntax-class"])], [text(cls_str(ci))]);

let id_view = (id): Node.t =>
  div([clss(["id"])], [text(string_of_int(id))]);

let extra_view = (id: int, ci: Core.Statics.t): Node.t =>
  div([clss(["extra"])], [id_view(id), cls_view(ci)]);

let print_ci = (ci, _) => {
  print_endline(Core.Statics.show(ci));
  switch (ci) {
  | InfoPat({mode, self, _})
  | InfoExp({mode, self, _}) =>
    Core.Statics.error_status(mode, self)
    |> Core.Statics.show_error_status
    |> print_endline
  | _ => ()
  };
  Event.Ignore;
};

let inspector_view = (id: int, ci: Core.Statics.t): Node.t =>
  div(
    [
      clss(
        ["cursor-inspector"] @ [Core.Statics.is_error(ci) ? errorc : happyc],
      ),
      Attr.on_click(print_ci(ci)),
    ],
    [
      extra_view(id, ci),
      view_of_info(ci),
      CtxInspector.inspector_view(id, ci),
    ],
  );

let view = (index': option(int), info_map: Core.Statics.map) => {
  let (index, ci) =
    switch (index') {
    | Some(index) => (index, Core.Id.Map.find_opt(index, info_map))
    | None => ((-1), None)
    };
  switch (ci) {
  | None => div([clss(["cursor-inspector"])], [text("No Static Data")])
  | Some(ci) => inspector_view(index, ci)
  };
};
