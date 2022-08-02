open Virtual_dom.Vdom;
open Node;
//open Core;

let ty_view = (cls: string, s: string): Node.t =>
  div([Attr.classes(["typ-view", cls])], [text(s)]);

let rec typ_view = (ty: Core.Typ.t): Node.t =>
  //TODO(andrew): parens on ops when ambiguous
  switch (ty) {
  | Unknown(_prov) => ty_view("unknown", "?")
  | Int => ty_view("Int", "Int")
  | Float => ty_view("Float", "Float")
  | Bool => ty_view("Bool", "Bool")
  | Arrow(t1, t2) =>
    div(
      [Attr.classes(["typ-view", "Arrow"])],
      [typ_view(t1), text("->"), typ_view(t2)],
    )
  | Prod(t1, t2) =>
    div(
      [Attr.classes(["typ-view", "Prod"])],
      [typ_view(t1), text(","), typ_view(t2)],
    )
  };

let error_view = (err: Core.Statics.error) =>
  switch (err) {
  | FreeVariable =>
    div([Attr.class_("err-free-variable")], [text("Free Variable")])
  | SynInconsistentBranches(_tys) =>
    //TODO: show types
    div(
      [Attr.classes(["error", "err-inconsistent-branches"])],
      [text("Inconsistent Branches")],
    )
  | TypeInconsistent(ty_ana, ty_syn) =>
    div(
      [Attr.classes(["error", "err-type-inconsistent"])],
      [typ_view(ty_ana), text("≉"), typ_view(ty_syn)],
    )
  };

let happy_view = (suc: Core.Statics.happy) => {
  switch (suc) {
  | SynConsistent(ty_syn) =>
    div(
      [Attr.classes(["happy", "syn-consistent"])],
      [text("⇒"), typ_view(ty_syn)],
    )
  | AnaConsistent(ty_ana, ty_syn, _ty_join) when ty_ana == ty_syn =>
    div(
      [Attr.classes(["happy", "ana-consistent-equal"])],
      [text("⇐"), typ_view(ty_ana)],
    )
  | AnaConsistent(ty_ana, ty_syn, _ty_join) =>
    div(
      [Attr.classes(["happy", "ana-consistent"])],
      [text("⇐"), typ_view(ty_syn), text("≈"), typ_view(ty_ana)],
    )
  | AnaInternalInconsistent(ty_ana, _)
  | AnaExternalInconsistent(ty_ana, _) =>
    div(
      [Attr.classes(["happy", "ana-consistent"])],
      [text("Ana Consistent*"), typ_view(ty_ana)],
    )
  };
};

let status_view = (err: Core.Statics.error_status) => {
  switch (err) {
  | InHole(error) => error_view(error)
  | NotInHole(happy) => happy_view(happy)
  };
};

let view_of_exp_cls = cls => Core.Term.UExp.show_cls(cls);

let view_of_pat_cls = cls => Core.Term.UPat.show_cls(cls);

let view_of_typ_cls = cls => Core.Term.UTyp.show_cls(cls);

let term_tag_sort = sort =>
  div([Attr.classes(["term-tag", "term-tag-" ++ sort])], [text(sort)]);

let view_of_info = (ci: Core.Statics.info): Node.t => {
  switch (ci) {
  | Invalid => div([], [text("No Info")])
  | InfoExp({mode, self, cls, _}) =>
    let error_status = Core.Statics.error_status(mode, self);
    div(
      [Attr.classes(["info", "exp"]), Attr.title(view_of_exp_cls(cls))],
      [term_tag_sort("exp"), status_view(error_status)],
    );
  | InfoPat({mode, self, cls, _}) =>
    let error_status = Core.Statics.error_status(mode, self);
    div(
      [Attr.classes(["info", "pat"]), Attr.title(view_of_pat_cls(cls))],
      [term_tag_sort("pat"), status_view(error_status)],
    );
  | InfoTyp({ty, cls, _}) =>
    div(
      [Attr.classes(["info", "typ"]), Attr.title(view_of_typ_cls(cls))],
      [term_tag_sort("typ"), typ_view(ty)],
    )
  };
};

let id_view = (id): Node.t =>
  div([Attr.classes(["id"])], [text(string_of_int(id))]);

let view = (id: int, ci: Core.Statics.info): Node.t =>
  div(
    [Attr.class_("cursor-inspector")],
    [id_view(id), view_of_info(ci)],
  );
