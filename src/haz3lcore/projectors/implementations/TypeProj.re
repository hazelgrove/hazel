open Virtual_dom.Vdom;
open Node;
open ProjectorBase;
open Language;
open Util;

let expected_ty = (info: option(Info.t)): option(Typ.t) =>
  switch (info) {
  | Some(InfoExp({ana, _}))
  | Some(InfoPat({ana, _})) => Some(ana)
  | _ => None
  };

let self_ty = (info: option(Info.t)): option(Typ.t) =>
  switch (info) {
  | Some(InfoExp({elab_syn_ty, _}))
  | Some(InfoPat({elab_syn_ty, _})) => Some(elab_syn_ty)
  | _ => None
  };

let totalize_ty = (expected_ty: option(Typ.t)): Typ.t =>
  switch (expected_ty) {
  | Some(expected_ty) => expected_ty
  | None => Typ.fresh(Unknown(Internal))
  };

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model =
    | Expected
    | Self
    | Dynamic;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type action =
    | ToggleDisplay;

  let init = (any: Any.t): option(model) => {
    switch (any) {
    | Exp(_)
    | Pat(_) => Some(Expected)
    | Any () => Some(Expected) /* Grout don't have sorts rn */
    | _ => None
    };
  };

  let elaborate_syntax = false;
  let focusable = Focusable.non;

  /* Whether statics has an expectation to show. An expression in synthetic
     position has none, so Expected mode falls back to Self and toggling
     skips it. */
  let has_expected = (statics: option(Info.t)): bool =>
    switch (expected_ty(statics)) {
    | None => false
    | Some(ty) => !Typ.is_syn(ty)
    };

  /* Whether the two readings are the same type. Not (==): that compares ids
     too, and statics builds types with Typ.temp, so two types it built
     compare equal on the Id.invalid sentinel while a written annotation --
     carrying its own tokens' ids -- never equals its synthesized twin. An
     expression with no expectation has nothing to agree with. */
  let readings_agree = (statics: option(Info.t)): bool =>
    has_expected(statics)
    && (
      switch (self_ty(statics), expected_ty(statics)) {
      | (Some(self), Some(expected)) => Typ.fast_equal(self, expected)
      | _ => false
      }
    );

  /* What the cell shows: the model says which reading was asked for, this
     says which it came to. Runtime carries no type -- its segment is built
     from the samples and comes with the ids to colour. */
  type content =
    | FromRuntime
    | OfTyp(Typ.t);

  /* The arrow, its tooltip and the content are one decision, so one cascade
     settles all three and none can drift from the others. */
  type reading = {
    glyph: string,
    description: string,
    content,
  };

  let reading = (model: model, statics: option(Info.t)): reading => {
    let self = () => {
      glyph: "⇒",
      description: "Self type",
      content: OfTyp(self_ty(statics) |> totalize_ty),
    };
    switch (model) {
    | Dynamic => {
        glyph: "⇓",
        description: "Dynamic type (from runtime values)",
        content: FromRuntime,
      }
    /* ↔ not ⇔: the bundled font has no bidirectional double arrow, and a
       fallback renders differently per browser (see proj-type.css). */
    | _ when readings_agree(statics) => {
        glyph: "↔",
        description: "Self type matches expected type",
        content: OfTyp(self_ty(statics) |> totalize_ty),
      }
    | Self => self()
    | Expected when !has_expected(statics) => self()
    | Expected => {
        glyph: "⇐",
        description: "Expected type",
        content: OfTyp(expected_ty(statics) |> totalize_ty),
      }
    };
  };

  let mode_view = (glyph, description) =>
    div(
      ~attrs=[Attr.classes(["mode"]), Attr.title(description)],
      [text(glyph)],
    );

  let typ_view = (content, info: info, utility, view_seg: View.seg, ~ctx) => {
    /* Dynamic hands over the exact segment its ids were computed from:
       preparing mints fresh paren ids, so a second one would not answer to
       them. */
    let (classes, seg) =
      switch (content) {
      | FromRuntime =>
        let (seg, dynamic_ids) =
          DynamicTypInfer.displayed_segment_and_dynamic_ids(
            ~typ_to_seg_with_diff_ids=
              utility.typ_to_seg_with_diff_ids(~inline=true),
            ~ctx,
            ~static_typ=self_ty(info.statics) |> totalize_ty,
            ~samples=
              switch (info.dynamics) {
              | None => []
              | Some(d: Dynamics.Info.t) => d.samples
              },
          );
        ((id => Id.Set.mem(id, dynamic_ids) ? ["dynamic"] : []), seg);
      | OfTyp(typ) => (
          (_ => []),
          utility.term_to_seg(~inline=true, Typ(typ)),
        )
      };
    div(
      ~attrs=[Attr.classes(["type-cell"])],
      [seg |> view_seg(~single_line=true, ~classes, Sort.Typ)],
    );
  };

  /* Statics are absent when the user has turned them off, and briefly while
     an edit is being checked. Say so rather than showing `?`, which would
     claim the type is unknown when what is unknown is whether we looked. */
  let unavailable_view = () =>
    div(
      ~attrs=[
        Attr.classes(["type-cell"]),
        Attr.title("No type information for this expression"),
      ],
      [text("unavailable")],
    );

  let update = (model, info, a: action) => {
    let has_expected = has_expected(info.statics);
    switch (a, model) {
    | (ToggleDisplay, Expected) => if (has_expected) {Self} else {Dynamic}
    | (ToggleDisplay, Self) => Dynamic
    | (ToggleDisplay, Dynamic) => if (has_expected) {Expected} else {Self}
    };
  };

  let placeholder = (_, _) => ProjectorCore.Shape.default;
  let error = (_, _): option(ProjectorBase.error) => None;

  let view = ({model, info, local, view_seg, _}: View.args(model, action)) =>
    View.{
      inline: div([]),
      offside:
        Some(
          div(
            ~attrs=[
              Attr.id(Id.cls(info.id)),
              Attr.tabindex(0),
              Attr.classes(["offside"]),
              Attr.on_double_click(_ => local(ToggleDisplay)),
            ],
            switch (info.statics) {
            | None => [unavailable_view()]
            | Some(statics) =>
              let {glyph, description, content} =
                reading(model, info.statics);
              [
                mode_view(glyph, description),
                typ_view(
                  content,
                  info,
                  info.utility,
                  view_seg,
                  ~ctx=Info.ctx_of(statics),
                ),
              ];
            },
          ),
        ),
      overlay: None,
      below: None,
      error: false,
    };
};
