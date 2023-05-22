open Virtual_dom.Vdom;
open Node;
open Util.Web;
open Haz3lcore;

let ty_view = (cls: string, s: string): Node.t =>
  div(~attr=clss(["typ-view", cls]), [text(s)]);

let prov_view: Haz3lcore.Typ.type_provenance => Node.t =
  fun
  | Inference(_) => div([])
  | Internal(_) => div([])
  | TypeHole(_) => div([])
  // div(~attr=clss(["typ-mod", "type-hole"]), [text("𝜏")])
  | SynSwitch(_) =>
    div(~attr=clss(["typ-mod", "syn-switch"]), [text("⇒")])
  | Anonymous => div([]);

let rec view =
        (~font_metrics: option(FontMetrics.t)=None, ty: Haz3lcore.Typ.t)
        : Node.t =>
  //TODO: parens on ops when ambiguous
  switch (ty) {
  | Unknown(_) =>
    switch (font_metrics) {
    | Some(font_metrics) =>
      div(
        ~attr=clss(["typ-view", "atom", "unknown"]),
        [
          EmptyHoleDec.relative_view(
            ~font_metrics,
            false,
            Haz3lcore.InferenceResult.hole_mold,
          ),
        ],
      )
    // div(~attr=clss(["typ-view", "atom", "unknown"]), [text("")])
    | _ => div(~attr=clss(["typ-view", "atom", "unknown"]), [text("")])
    }

  | Int => ty_view("Int", "Int")
  | Float => ty_view("Float", "Float")
  | String => ty_view("String", "String")
  | Bool => ty_view("Bool", "Bool")
  | Var(name) => ty_view("Var", name)
  | List(t) =>
    div(
      ~attr=clss(["typ-view", "atom", "List"]),
      [text("["), view(~font_metrics, t), text("]")],
    )
  | Arrow(t1, t2) =>
    div(
      ~attr=clss(["typ-view", "Arrow"]),
      [view(~font_metrics, t1), text("->"), view(~font_metrics, t2)],
    )
  | Prod([]) => div(~attr=clss(["typ-view", "Prod"]), [text("()")])
  | Prod([_]) =>
    div(~attr=clss(["typ-view", "Prod"]), [text("BadProduct")])
  | Prod([t0, ...ts]) =>
    div(
      ~attr=clss(["typ-view", "atom", "Prod"]),
      [
        text("("),
        div(
          ~attr=clss(["typ-view", "Prod"]),
          [view(~font_metrics, t0)]
          @ (
            List.map(t => [text(","), view(~font_metrics, t)], ts)
            |> List.flatten
          ),
        ),
        text(")"),
      ],
    )
  | Sum(t1, t2) =>
    div(
      ~attr=clss(["typ-view", "Sum"]),
      [view(~font_metrics, t1), text("+"), view(~font_metrics, t2)],
    )
  };

let rec view_of_pts =
        (
          ~font_metrics,
          outermost,
          with_cls,
          potential_typ_set: PotentialTypeSet.t,
        )
        : Node.t => {
  switch (potential_typ_set) {
  | [] => view(Typ.Unknown(Anonymous))
  | [hd] => view_of_ptyp(~font_metrics, outermost, with_cls, hd)
  | _ =>
    div(
      ~attr=clss(with_cls ? ["typ-view", "atom", "unknown"] : []),
      [
        EmptyHoleDec.relative_view(
          ~font_metrics,
          true,
          Haz3lcore.InferenceResult.hole_mold,
        ),
      ],
    )
  };
}
and view_of_ptyp =
    (
      ~font_metrics,
      outermost: bool,
      with_cls: bool,
      potential_typ: PotentialTypeSet.potential_typ,
    )
    : Node.t =>
  switch (potential_typ) {
  | Base(btyp) => view_of_btyp(btyp, with_cls)
  | Binary(ctor, potential_typ_set_lt, potential_typ_set_rt) =>
    let (ctor_start, ctor_string, ctor_end, cls) =
      switch (ctor) {
      | CArrow =>
        outermost
          ? ("", " -> ", "", ["typ-view", "Arrow"])
          : ("(", " -> ", ")", ["typ-view", "Arrow"])
      | CProd => ("(", ", ", ")", ["typ-view", "Sum"])
      | CSum =>
        outermost
          ? ("", " + ", "", ["typ-view", "Sum"])
          : ("(", " + ", ")", ["typ-view", "Sum"])
      };
    let cls = with_cls ? cls : [];
    div(
      ~attr=clss(cls),
      [
        text(ctor_start),
        view_of_pts(~font_metrics, false, with_cls, potential_typ_set_lt),
        text(ctor_string),
        view_of_pts(~font_metrics, false, with_cls, potential_typ_set_rt),
        text(ctor_end),
      ],
    );
  | Unary(ctor, potential_typ_set) =>
    let (start_text, end_text, cls) =
      switch (ctor) {
      | CList => ("[", "]", ["typ-view", "atom", "List"])
      };
    let cls = with_cls ? cls : [];
    div(
      ~attr=clss(cls),
      [
        text(start_text),
        view_of_pts(~font_metrics, false, with_cls, potential_typ_set),
        text(end_text),
      ],
    );
  }
and view_of_btyp = (btyp: PotentialTypeSet.base_typ, with_cls: bool): Node.t => {
  with_cls
    ? btyp |> PotentialTypeSet.string_of_btyp |> text
    : btyp |> PotentialTypeSet.base_typ_to_ityp |> ITyp.ityp_to_typ |> view;
};
