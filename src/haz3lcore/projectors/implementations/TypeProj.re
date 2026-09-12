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

/* The segment to display in Dynamic mode, and the ids of its tokens that
   came from runtime. [ctx] must be the expression's own: the types a sample
   mentions are the ones in scope where it was sampled, and a stand-in
   context reports them as something else. */
let get_dynamic_segment =
    (utility: utility, info: info, ~ctx: Ctx.t): (Base.segment, Id.Set.t) =>
  DynamicTypInfer.displayed_segment_and_dynamic_ids(
    ~typ_to_seg_with_diff_ids=utility.typ_to_seg_with_diff_ids(~inline=true),
    ~ctx,
    ~static_typ=
      Option.value(
        ~default=Typ.fresh(Unknown(Internal)),
        self_ty(info.statics),
      ),
    ~samples=
      switch (info.dynamics) {
      | None => []
      | Some(d: Dynamics.Info.t) => d.samples
      },
  );

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

  /* The arrow and the tooltip describe the same reading, so one cascade
     decides both. The model says which reading was asked for; this says
     which it came to, since Expected falls back to Self where there is no
     expectation, and either reads as agreement where the two coincide. */
  type mode = {
    glyph: string,
    description: string,
  };

  let mode = (model: model, statics: option(Language.Info.t)): mode =>
    switch (model) {
    | Dynamic => {
        glyph: "⇓",
        description: "Dynamic type (from runtime values)",
      }
    /* ↔ not ⇔: the bundled font has no bidirectional double arrow, and a
       fallback renders differently per browser (see proj-type.css). */
    | _ when self_ty(statics) == expected_ty(statics) => {
        glyph: "↔",
        description: "Self type matches expected type",
      }
    | _ when expected_ty(statics) |> totalize_ty |> Typ.is_syn => {
        glyph: "⇒",
        description: "Self type",
      }
    | Self => {
        glyph: "⇒",
        description: "Self type",
      }
    | Expected => {
        glyph: "⇐",
        description: "Expected type",
      }
    };

  let mode_view = (model, info) => {
    let {glyph, description} = mode(model, info);
    div(
      ~attrs=[Attr.classes(["mode"]), Attr.title(description)],
      [text(glyph)],
    );
  };

  let typ_view = (model, info: info, utility, view_seg: View.seg) => {
    /* Every arm yields its own segment, so Dynamic can hand over the exact
       one its ids were computed from. */
    let to_seg = (t: Typ.t) => utility.term_to_seg(~inline=true, Typ(t));
    let cell = (~attrs=[], contents) =>
      div(~attrs=[Attr.classes(["type-cell"]), ...attrs], contents);
    switch (info.statics) {
    /* Statics are absent when the user has turned them off, and briefly
       while an edit is being checked. Say so rather than showing `?`, which
       would claim the type is unknown when what is unknown is whether we
       looked. */
    | None =>
      cell(
        ~attrs=[Attr.title("No type information for this expression")],
        [text("unavailable")],
      )
    | Some(statics) =>
      let (classes, seg) =
        switch (model) {
        | Dynamic =>
          let (seg, dynamic_ids) =
            get_dynamic_segment(utility, info, ~ctx=Info.ctx_of(statics));
          ((id => Id.Set.mem(id, dynamic_ids) ? ["dynamic"] : []), seg);
        | Expected when expected_ty(info.statics) |> totalize_ty |> Typ.is_syn => (
            (_ => []),
            to_seg(self_ty(info.statics) |> totalize_ty),
          )
        | Expected => (
            (_ => []),
            to_seg(expected_ty(info.statics) |> totalize_ty),
          )
        | Self => ((_ => []), to_seg(self_ty(info.statics) |> totalize_ty))
        };
      cell([seg |> view_seg(~single_line=true, ~classes, Sort.Typ)]);
    };
  };

  let update = (model, info, a: action) => {
    let has_expected =
      switch (expected_ty(info.statics)) {
      | Some(ty) => !Typ.is_syn(ty)
      | None => false
      };
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
            [
              mode_view(model, info.statics),
              typ_view(model, info, info.utility, view_seg),
            ],
          ),
        ),
      overlay: None,
      below: None,
      error: false,
    };
};
